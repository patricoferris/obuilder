open Lwt.Infix
open Sexplib.Std

type env = (string * string) list [@@deriving sexp]

let ( >>!= ) = Lwt_result.bind

type unix_fd = {
  raw : Unix.file_descr;
  mutable needs_close : bool;
}

let close fd =
  assert (fd.needs_close);
  Unix.close fd.raw;
  fd.needs_close <- false

let ensure_closed_unix fd =
  if fd.needs_close then close fd

let ensure_closed_lwt fd =
  if Lwt_unix.state fd = Lwt_unix.Closed then Lwt.return_unit
  else Lwt_unix.close fd

let pp_signal f x =
  let open Sys in
  if x = sigkill then Fmt.string f "kill"
  else if x = sigterm then Fmt.string f "term"
  else Fmt.int f x

let pp_cmd = Fmt.box Fmt.(list ~sep:sp (quote string))

let redirection = function
  | `FD_move_safely x -> `FD_copy x.raw
  | `Dev_null -> `Dev_null

let close_redirection (x : [`FD_move_safely of unix_fd | `Dev_null]) =
  match x with
  | `FD_move_safely x -> ensure_closed_unix x
  | `Dev_null -> ()

(* stdin, stdout and stderr are copied to the child and then closed on the host.
   They are closed at most once, so duplicates are OK. *)
let default_exec ?cwd ?stdin ?stdout ?stderr ~pp argv =
  let proc =
    let stdin  = Option.map redirection stdin in
    let stdout = Option.map redirection stdout in
    let stderr = Option.map redirection stderr in
    Lwt_process.exec ?cwd ?stdin ?stdout ?stderr argv
  in
  Option.iter close_redirection stdin;
  Option.iter close_redirection stdout;
  Option.iter close_redirection stderr;
  proc >|= function
  | Unix.WEXITED n -> Ok n
  | Unix.WSIGNALED x -> Fmt.error_msg "%t failed with signal %d" pp x
  | Unix.WSTOPPED x -> Fmt.error_msg "%t stopped with signal %a" pp pp_signal x

(* Overridden in unit-tests *)
let lwt_process_exec = ref default_exec

let exec_result ?cwd ?stdin ?stdout ?stderr ~pp argv =
  Logs.info (fun f -> f "Exec %a" pp_cmd argv);
  !lwt_process_exec ?cwd ?stdin ?stdout ?stderr ~pp ("", Array.of_list argv) >>= function
  | Ok 0 -> Lwt_result.return ()
  | Ok n -> Lwt.return @@ Fmt.error_msg "%t failed with exit status %d" pp n
  | Error e -> Lwt_result.fail (e : [`Msg of string] :> [> `Msg of string])

let exec ?cwd ?stdin ?stdout ?stderr argv =
  Logs.info (fun f -> f "Exec %a" pp_cmd argv);
  let pp f = pp_cmd f argv in
  !lwt_process_exec ?cwd ?stdin ?stdout ?stderr ~pp ("", Array.of_list argv) >>= function
  | Ok 0 -> Lwt.return_unit
  | Ok n -> Lwt.fail_with (Fmt.strf "%t failed with exit status %d" pp n)
  | Error (`Msg m) -> Lwt.fail (Failure m)

let running_as_root = Unix.getuid () = 0

let sudo ?stdin args =
  let env = if List.hd args = "zfs" then [ "DYLD_FALLBACK_LIBRARY_PATH=/Users/patrickferris/bin" ] else [] in 
  let args = if running_as_root then args else "sudo" :: env @ args in
  exec ?stdin args

let sudo_result ?cwd ?stdin ?stdout ?stderr ~pp args =
  let args = if running_as_root then args else "sudo" :: args in
  exec_result ?cwd ?stdin ?stdout ?stderr ~pp args

let rec write_all fd buf ofs len =
  assert (len >= 0);
  if len = 0 then Lwt.return_unit
  else (
    Lwt_unix.write fd buf ofs len >>= fun n ->
    write_all fd buf (ofs + n) (len - n)
  )

let write_file ~path contents =
  Lwt_io.(with_file ~mode:output) path @@ fun ch ->
  Lwt_io.write ch contents

let with_pipe_from_child fn =
  let r, w = Lwt_unix.pipe_in () in
  let w = { raw = w; needs_close = true } in
  Lwt.finalize
    (fun () ->
       Lwt_unix.set_close_on_exec r;
       fn ~r ~w
    )
    (fun () ->
       ensure_closed_unix w;
       ensure_closed_lwt r
    )

let with_pipe_to_child fn =
  let r, w = Lwt_unix.pipe_out () in
  let r = { raw = r; needs_close = true } in
  Lwt.finalize
    (fun () ->
       Lwt_unix.set_close_on_exec w;
       fn ~r ~w
    )
    (fun () ->
       ensure_closed_unix r;
       ensure_closed_lwt w
    )

let with_pipe_between_children fn =
  let r, w = Unix.pipe ~cloexec:true () in
  let r = { raw = r; needs_close = true } in
  let w = { raw = w; needs_close = true } in
  Lwt.finalize
    (fun () -> fn ~r ~w)
    (fun () ->
       ensure_closed_unix r;
       ensure_closed_unix w;
       Lwt.return_unit
    )

let pread ?stderr argv =
  with_pipe_from_child @@ fun ~r ~w ->
  let child = exec ~stdout:(`FD_move_safely w) ?stderr argv in
  let r = Lwt_io.(of_fd ~mode:input) r in
  Lwt.finalize
    (fun () -> Lwt_io.read r)
    (fun () -> Lwt_io.close r)
  >>= fun data ->
  child >>= fun () ->
  Lwt.return data

let check_dir x =
  match Unix.lstat x with
  | Unix.{ st_kind = S_DIR; _ } -> `Present
  | _ -> Fmt.failwith "Exists, but is not a directory: %S" x
  | exception Unix.Unix_error(Unix.ENOENT, _, _) -> `Missing

let rec mkdir_p path =
  let dirname = Filename.dirname path in
  if not (String.equal dirname path) then begin
    ensure_dir dirname;
  end;
  Unix.mkdir path 0o777

and ensure_dir path =
  match check_dir path with
  | `Present -> ()
  | `Missing -> mkdir_p path

module Macos = struct 
  let ( / ) = Filename.concat  

  let user_exists ~user =
    pread ["sudo"; "dscl"; "."; "list"; "/Users"] >|= fun s -> 
    List.exists (Astring.String.equal user) (Astring.String.cuts ~sep:"\n" s)

  (* Generates a new MacOS user called `<prefix><uid>' *)
  let create_new_user ~username ~home ~uid ~gid = 
    user_exists ~user:username >>= begin function 
      | true ->  Lwt.return_ok () 
      | false -> 
        let user = "/Users" / username in  
        let pp s ppf = Fmt.pf ppf "[ Mac ] %s\n" s in 
        let dscl = ["dscl"; "."; "-create"; user ] in 
          sudo_result ~pp:(pp "UniqueID") (dscl @ ["UniqueID"; uid]) >>!= fun _ ->
          sudo_result ~pp:(pp "PrimaryGroupID") (dscl @ ["PrimaryGroupID"; gid]) >>!= fun _ ->
          sudo_result ~pp:(pp "UserShell") (dscl @ ["UserShell"; "/bin/bash"]) >>!= fun _ -> 
          sudo_result ~pp:(pp "NFSHomeDirectory") (dscl @ ["NFSHomeDirectory"; home]) >>!= fun _ -> 
          sudo (dscl @ ["-passwd"; user; "hello"]) >>= fun _ -> Lwt.return_ok ()
    end 

  let delete_user ~user = 
    user_exists ~user >>= begin function
      | false -> 
        Log.info (fun f -> f "Not deleting %s as they don't exists" user);
        Lwt_result.return ()
      | true ->
        let user = "/Users" / user in 
        let pp s ppf = Fmt.pf ppf "[ Mac ] %s\n" s in 
        let delete = ["dscl"; "."; "-delete"; user ] in 
          sudo_result ~pp:(pp "Deleting") delete
    end

  (* HACK: I couldn't find a way to get the PID of the running command for the macOS sandbox.
   * So here we list processes and match on the command... to get the PID... *)
  let find_pid ~cmd =
    pread ["sudo"; "ps"; "-eo"; "pid,args"] >|= fun s ->
    let lines = Astring.String.cuts ~sep:"\n" s in
    let pid_arg s =
      let lst = Astring.String.cuts ~sep:" " s in
      let lst = List.filter (fun s -> String.length s <> 0) lst in
      if List.length lst > 2 then Some (List.hd lst, String.concat " " (List.tl lst)) else None
    in
    let pid_args = List.filter_map pid_arg lines in
    Option.map fst (List.find_opt (fun (_, c) -> Log.info (fun f -> f "Does %s match %s" cmd c); String.equal cmd c) pid_args)

  let descendants ~pid =
    if String.length pid = 0 then Lwt.return []
    else
    Lwt.catch
      (fun () -> pread ["sudo"; "pgrep"; "-P"; pid ] >|= fun s -> Astring.String.cuts ~sep:"\n" s)
      (* Errors if there are none, probably errors for other reasons too... *)
      (fun _ -> Lwt.return [])

  let pkill ~pid =
    let pp s ppf = Fmt.pf ppf "[ %s ]" s in
    if String.length pid = 0 then (Log.warn (fun f -> f "Empty PID"); Lwt.return ())
    else begin
      let delete = ["pkill"; "-9"; "-P"; pid ] in
      sudo_result ~pp:(pp "PKILL") delete >>= fun t ->
        match t with
        | Ok () -> Lwt.return ()
        | Error (`Msg m) -> (
          Log.warn (fun f -> f "Failed to pkill for %s because %s" pid m);
          Lwt.return ()
        )
    end

  let kill_all_descendants ~pid =
    let rec kill pid : unit Lwt.t =
      descendants ~pid >>= fun ds ->
      Lwt_list.iter_s kill ds >>= fun () ->
      pkill ~pid
    in
      kill pid

  let copy_template ~base ~local =
    let pp s ppf = Fmt.pf ppf "[ %s ]" s in
    sudo_result ~pp:(pp "RSYNC") ["rsync"; "-avq"; base ^ "/"; local]

  let change_home_directory_for ~user ~homedir = 
    ["dscl"; "."; "-create"; "/Users/" ^ user ; "NFSHomeDirectory"; homedir ]

  (* Used by the FUSE filesystem to indicate where a users home directory should be ...*)
  let update_scoreboard ~uid ~scoreboard ~homedir = 
    ["ln"; "-Fhs"; homedir; scoreboard ^ "/" ^ string_of_int uid]

  let remove_link ~uid ~scoreboard =
    [ "rm"; scoreboard ^ "/" ^ string_of_int uid ]

  let get_tmpdir ~user = 
    ["sudo"; "-u"; user; "-i"; "getconf"; "DARWIN_USER_TEMP_DIR"]
end 
