open Lwt.Infix 

type t = {
  uid: int;
  gid: int;
  (* Where zfs dynamic libraries are -- can't be in /usr/local/lib 
     see notes in .mli file under "Various Gotchas"... *)
  fallback_library_path : string;
  (* Scoreboard -- where we keep our symlinks for knowing homedirs for users *)
  scoreboard : string;
}

let create ~uid ~fallback_library_path ~scoreboard = { 
  uid; 
  gid = 1000; 
  fallback_library_path;
  scoreboard;
}

let ( / ) = Filename.concat 

let run_as ~env ~cwd ~user ~cmd = 
  let cwd = "." ^ cwd in 
  (* For handling `Workdir' statements - make and change *)
  let cwd cmd = "mkdir -p " ^ cwd ^ " && cd " ^ cwd ^ " && " ^ String.concat " " cmd in 
  let command = 
    if List.hd cmd = "rsync" 
    then ["sudo";] @ cmd
    else begin 
      match cmd with 
        | "/bin/bash" :: "-c" :: cmd -> ["sudo"; "-u"; user; "-i"; "env"] @ env @ [ "/bin/bash"; "-c"; cwd cmd]
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

let from ~log ~from_stage t =
  log `Heading (Fmt.strf "SYS %s" (snd from_stage));
  let id = Sha256.to_hex (Sha256.string (snd from_stage)) in
  let home = "/Volumes/tank/result" / id in 
  fun ~cancelled:_ ~log:_ _ ->
    Os.Macos.create_new_user ~prefix:"mac" ~home ~uid:(string_of_int t.uid) ~gid:"1000" >>= fun _ ->
    Os.Macos.copy_brew_template ~lib:home ~local:home >>= fun _ -> 
    Os.sudo [ "chown"; "-R"; ":1000"; home ] >>= fun () -> 
    Lwt.return (Ok ())

(* XXX Patricoferris: there must be a better way to deal with this! *)
let convert_env env = 
  let paths = List.filter (fun (k, _) -> String.equal k "PATH") env in 
  let paths = 
    List.map (fun (_, p) -> 
      match Astring.String.cut ~sep:":" p with 
        | Some (path, "$PATH") -> path
        | _ -> "") paths 
  in 
  let paths = "PATH=" ^ String.concat ":" paths ^ ":$PATH" in
  let rec aux acc = function 
    | []  -> List.rev acc 
    | ("PATH", _) :: xs -> aux acc xs (* Remove the paths *)
    | (k, v)::xs -> aux ((k ^ "=" ^ v) :: acc) xs 
  in 
   (paths :: aux [] env) (* Add the filtered paths *)

(* A build step in macos: 
   - Should be properly sandboxed using sandbox-exec (coming soon...)
   - Umask g+w to work across users if restored from a snapshot
   - Set the new home directory of the user, to the new hash
   - Should be executed by the underlying user (t.uid) *)
let run ~cancelled ?stdin:stdin ~log t config homedir =
  Os.with_pipe_from_child @@ fun ~r:out_r ~w:out_w ->
  let set_homedir = Os.Macos.change_home_directory_for ~user:("mac" ^ string_of_int t.uid) ~homedir in 
  let switch_prefix = ("OPAM_SWITCH_PREFIX", homedir / ".opam" / "default") in 
  let bin_prefix = ("PATH", homedir / ".opam" / "default" / "bin" ^ ":$PATH") in 
  let env = convert_env (("HOME", homedir) (* :: ("TMPDIR", homedir / "tmp") *) :: bin_prefix :: [ switch_prefix ]) in 
  let update_scoreboard = Os.Macos.update_scoreboard ~uid:t.uid ~homedir ~scoreboard:t.scoreboard in 
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
          let pp f = Fmt.pf f "Should kill %S" homedir in
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