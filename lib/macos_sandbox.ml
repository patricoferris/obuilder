open Lwt.Infix 

type t = {
  uid: int;
  gid: int;
}

let ( / ) = Filename.concat

let exec ?(with_sandbox=false) user cmd = 
  match with_sandbox with 
    | true -> [ "" ] 
    | false -> [ "sudo"; "-u"; user; "-i" ] @ cmd


let copy_to_log ~src ~dst =
  let buf = Bytes.create 4096 in
  let rec aux () =
    Lwt_unix.read src buf 0 (Bytes.length buf) >>= function
    | 0 -> Lwt.return_unit
    | n -> Build_log.write dst (Bytes.sub_string buf 0 n) >>= aux
  in
  aux ()

(* A build step in macos: 
   - Should be properly sandboxed using sandbox-exec (coming soon...)
   - Umask g+w to work across users if restored from a snapshot
   - Set the new home directory of the user, to the new hash
   - Should be executed by the underlying user (t.uid) *)
let run ~cancelled ?stdin:stdin ~log ~hash t config _ =
  Os.with_pipe_from_child @@ fun ~r:out_r ~w:out_w ->
  let homedir = "/zfs/" / hash in 
  let set_homedir = Os.Macos.change_home_directory_for ~user:(string_of_int t.uid) ~homedir in 
  let update_scoreboard () = Os.Macos.update_scoreboard ~uid:t.uid ~homedir in 
  let cmd = exec (string_of_int t.uid) config.Config.argv in
  let stdout = `FD_move_safely out_w in
  let stderr = stdout in
  let copy_log = copy_to_log ~src:out_r ~dst:log in
  let proc =
    let stdin = Option.map (fun x -> `FD_move_safely x) stdin in
    let pp f = Os.pp_cmd f config.argv in
    Os.sudo_result ~pp set_homedir >>= fun _ ->
    Os.sudo_result ~pp (update_scoreboard ()) >>= fun _ ->
    Os.sudo_result ?stdin ~stdout ~stderr ~pp cmd
  in
  Os.close out_w;
  Option.iter Os.close stdin;
  Lwt.on_termination cancelled (fun () ->
      (* XXX patricoferris: terminate stuff & cleanup?... *)
      ()
    );
  proc >>= fun r ->
  copy_log >>= fun () ->
  if Lwt.is_sleeping cancelled then Lwt.return (r :> (unit, [`Msg of string | `Cancelled]) result)
  else Lwt_result.fail `Cancelled