open Lwt.Infix 

type t = {
  uid: int;
  gid: int;
}

let create ~uid = { uid; gid = 1000 }

let ( / ) = Filename.concat

let run_as ~cwd ~user ~cmd = 
  let cwd = "./" ^ cwd in 
  let cwd cmd = "mkdir -p " ^ cwd ^ " && cd " ^ cwd ^ " && " ^ String.concat " " cmd in 
  let command = 
    if List.hd cmd = "rsync" 
    then ["sudo";] @ cmd
    else begin 
      match cmd with 
        | "/bin/bash" :: "-c" :: cmd -> ["sudo"; "-u"; user; "-i"; "/bin/bash"; "-c"; cwd cmd]
        | cmd -> ["sudo"; "-u"; user; "-i"; "/bin/bash"; "-c"; cwd cmd]
    end 
  in
  Log.debug (fun f -> f "Running: %s" (String.concat " " command)); 
    command 


let copy_to_log ~src ~dst =
  let buf = Bytes.create 4096 in
  let rec aux () =
    Lwt_unix.read src buf 0 (Bytes.length buf) >>= function
    | 0 -> Lwt.return_unit
    | n -> Build_log.write dst (Bytes.sub_string buf 0 n) >>= aux
  in
  aux ()

let zfs_home_dir hash = 
  "/Volumes/tank/result" / hash 

(* A build step in macos: 
   - Should be properly sandboxed using sandbox-exec (coming soon...)
   - Umask g+w to work across users if restored from a snapshot
   - Set the new home directory of the user, to the new hash
   - Should be executed by the underlying user (t.uid) *)
let run ~cancelled ?stdin:stdin ~log ~hash t config _ =
  Os.with_pipe_from_child @@ fun ~r:out_r ~w:out_w ->
  let homedir = zfs_home_dir hash in 
  let set_homedir = Os.Macos.change_home_directory_for ~user:("mac" ^ string_of_int t.uid) ~homedir in 
  let _env = Os.convert_env config.Config.env in 
  let update_scoreboard = Os.Macos.update_scoreboard ~uid:t.uid ~homedir in 
  let cmd = run_as ~cwd:config.Config.cwd ~user:("mac" ^ string_of_int t.uid) ~cmd:config.Config.argv in
  let stdout = `FD_move_safely out_w in
  let stderr = stdout in
  let copy_log = copy_to_log ~src:out_r ~dst:log in
  let proc =
    let stdin = Option.map (fun x -> `FD_move_safely x) stdin in
    let pp f = Os.pp_cmd f config.argv in
    Os.sudo_result ~pp set_homedir >>= fun _ ->
    Os.sudo_result ~pp update_scoreboard >>= fun _ ->
    Os.sudo_result ~pp ["umask"; "-S"] >>= fun _ -> 
    Os.exec_result ?stdin ~stdout ~stderr ~pp cmd
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