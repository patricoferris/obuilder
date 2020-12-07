open Lwt.Infix 

type t = {
  uid: int;
  gid: int;
}

let create ~uid = { uid; gid = 1000 }

let run_as ~env ~cwd ~user ~cmd = 
  let cwd = "." ^ cwd in 
  let cwd cmd = "mkdir -p " ^ cwd ^ " && cd " ^ cwd ^ " && " ^ String.concat " " cmd in 
  let command = 
    if List.hd cmd = "rsync" 
    then ["sudo";] @ cmd
    else begin 
      match cmd with 
        | "/bin/bash" :: "-c" :: cmd -> ["sudo"; "-u"; user; "-i"; "env"] @ (env |> Array.to_list) @ [ "/bin/bash"; "-c"; cwd cmd]
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


(* A build step in macos: 
   - Should be properly sandboxed using sandbox-exec (coming soon...)
   - Umask g+w to work across users if restored from a snapshot
   - Set the new home directory of the user, to the new hash
   - Should be executed by the underlying user (t.uid) *)
let run ~cancelled ?stdin:stdin ~log ~hash t config homedir =
  let ( / ) = Filename.concat in 
  Os.with_pipe_from_child @@ fun ~r:out_r ~w:out_w ->
  let set_homedir = Os.Macos.change_home_directory_for ~user:("mac" ^ string_of_int t.uid) ~homedir in 
  let ocaml_path = "/Users/patrickferris/ocaml/4.11.0/bin" in 
  let switch_prefix = ("OPAM_SWITCH_PREFIX", homedir / ".opam" / "default") in 
  let bin_prefix = ("PATH", ocaml_path ^ ":" ^ homedir / ".opam" / "default" / "bin" ^ ":$PATH") in 
  let env = Os.convert_env (("HOME", homedir) (* :: ("TMPDIR", homedir / "tmp") *) :: bin_prefix :: [ switch_prefix ]) in 
  let update_scoreboard = Os.Macos.update_scoreboard ~uid:t.uid ~homedir in 
  let cmd = run_as ~env ~cwd:config.Config.cwd ~user:("mac" ^ string_of_int t.uid) ~cmd:config.Config.argv in
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
  Lwt.on_termination cancelled (fun () ->
  let rec aux () =
        if Lwt.is_sleeping proc then (
          let pp f = Fmt.pf f "Should kill %S" hash in
          Os.sudo_result ["echo"; "TODO"] ~pp >>= function
          | Ok () -> Lwt.return_unit
          | Error (`Msg m) ->
            (* This might be because it hasn't been created yet, so retry. *)
            Log.warn (fun f -> f "kill failed: %s (will retry in 10s)" m);
            Lwt_unix.sleep 10.0 >>= aux
        ) else Lwt.return_unit  (* Process has already finished *)
      in
      Lwt.async aux
    );
  proc >>= fun r -> 
  Os.close out_w; 
  Option.iter Os.close stdin; 
  copy_log >>= fun () ->
  if Lwt.is_sleeping cancelled then Lwt.return (r :> (unit, [`Msg of string | `Cancelled]) result)
  else Lwt_result.fail `Cancelled