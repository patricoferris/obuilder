open Lwt.Infix 
open Cmdliner 

type t = {
  uid: int;
  gid: int;
  mutable tmpdir : string;
  mutable user : string;
  (* Where zfs dynamic libraries are -- can't be in /usr/local/lib 
     see notes in .mli file under "Various Gotchas"... *)
  fallback_library_path : string;
  (* Scoreboard -- where we keep our symlinks for knowing homedirs for users *)
  scoreboard : string;
}

open Sexplib.Conv

type config = { 
  uid: int;
  fallback_library_path : string;
  scoreboard : string;
}[@@deriving sexp]

let version = "macos-sandboxing"


let ( / ) = Filename.concat 

let run_as ~user ~cmd =
  let command = 
    "su" :: "-l" :: user :: "-c" :: "--" :: {|"$0" "$@"|} :: cmd
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

let user_name ~prefix ~uid ~from = 
  Fmt.str "%s%s-%s" prefix uid from

let from ~log ~from_stage (t : t) =
  log `Heading (Fmt.strf "SYS %s" from_stage);
  let id = Sha256.to_hex (Sha256.string from_stage) in
  let home = "/Volumes/tank/result" / id in 
  let uid = string_of_int t.uid in 
  let username = user_name ~prefix:"mac" ~uid ~from:from_stage in 
  t.user <- username;
  fun ~cancelled:_ ~log:_ (_ : string) ->
    Os.Macos.create_new_user ~username ~home ~uid ~gid:"1000" >>= fun _ ->
    Os.Macos.copy_template ~base:("/Users/" ^ from_stage) ~local:home >>= fun _ -> 
    Os.(sudo @@ Macos.update_scoreboard ~uid:t.uid ~homedir:home ~scoreboard:t.scoreboard) >>= fun _ ->
    Os.sudo [ "chown"; "-R"; ":1000"; home ] >>= fun () -> 
    Os.pread @@ Os.Macos.get_tmpdir ~user:username >>= fun s -> 
    Log.info (fun f -> f "Setting temporary directory to %s" s);
    t.tmpdir <- s; 
    Lwt.return (Ok () :> (unit, [ `Cancelled | `Msg of string ]) result)

let clean (t : t) = 
  (* Os.(sudo (Macos.remove_link ~uid:t.uid ~scoreboard:t.scoreboard)) >>= fun () -> 
  Log.info (fun f -> f "Deleting user %s" t.user);
  Os.Macos.delete_user ~user:t.user >|= function 
    | Ok () -> ()
    | _ -> Log.err (fun f -> f "Failed to delete user: %s" t.user); () *)
  Lwt.return ()

(* A build step in macos: 
   - Should be properly sandboxed using sandbox-exec (coming soon...)
   - Umask g+w to work across users if restored from a snapshot
   - Set the new home directory of the user, to the new hash
   - Should be executed by the underlying user (t.uid) *)
let run ~cancelled ?stdin:stdin ~log (t : t) config homedir =
  Os.with_pipe_from_child @@ fun ~r:out_r ~w:out_w ->
  let user = t.user in 
  let uid = string_of_int t.uid in 
  Os.Macos.create_new_user ~username:user ~home:homedir ~uid ~gid:"1000" >>= fun _ -> 
  let set_homedir = Os.Macos.change_home_directory_for ~user ~homedir in 
  let update_scoreboard = Os.Macos.update_scoreboard ~uid:t.uid ~homedir ~scoreboard:t.scoreboard in
  let stdout = `FD_move_safely out_w in
  let stderr = stdout in
  let copy_log = copy_to_log ~src:out_r ~dst:log in
  let proc =
    let stdin = Option.map (fun x -> `FD_move_safely x) stdin in
    let pp f = Os.pp_cmd f config.argv in
    Os.sudo_result ~pp set_homedir >>= fun _ ->
    Os.sudo_result ~pp update_scoreboard >>= fun _ ->
    let cmd = run_as ~user ~cmd:config.Config.argv in
    Os.ensure_dir config.Config.cwd;
    Os.exec_result ?stdin ~stdout ~stderr ~pp ~cwd:config.Config.cwd cmd
  in
  Lwt.on_termination cancelled (fun () ->
  let rec aux () =
        if Lwt.is_sleeping proc then (
          let pp f = Fmt.pf f "Should kill %S" homedir in
          (* XXX patricoferris: Pkill processes belonging to user then deleter user? *)
          Os.Macos.pkill ~user:t.user
          (*clean t*)
        ) else Lwt.return_unit  (* Process has already finished *)
      in
      Lwt.async aux
    );
  proc >>= fun r -> 
  copy_log >>= fun () ->
  begin match r with 
    (* Failed builds should delete the builder user in case the next build is not the same user
       but maybe the same UID. *)
    | Error (`Msg _) -> 
      (* clean t >>= fun () -> Lwt.return () *) Lwt.return ()
    | _ -> Lwt.return ()
  end >>= fun () ->
      if Lwt.is_sleeping cancelled then Lwt.return (r :> (unit, [`Msg of string | `Cancelled]) result)
      else Lwt_result.fail `Cancelled

let post_build () =
  let f = ["umount"; "-f"; "/usr/local"] in 
  let pp ppf = Fmt.pf ppf "[ OSXFUSE ] " in 
  Os.sudo_result ~pp f >>= fun _ -> Lwt.return ()

let rec pre_build () =
  let f = [ "obuilderfs"; "/Users/administrator/scoreboard"; "/usr/local"; "-o"; "allow_other" ] in
  let pp ppf = Fmt.pf ppf "[ OSXFUSE ] " in
  Os.sudo_result ~pp f >>= function Ok _ -> Lwt.return () | Error (`Msg m) -> (post_build () >>= fun _ -> pre_build ())

let create ?state_dir:_ c = 
  {
    uid = c.uid;
    gid = 1000;
    tmpdir = "";
    user = "";
    fallback_library_path = c.fallback_library_path;
    scoreboard = c.scoreboard;
  }

let uid =
  Arg.required @@
  Arg.opt Arg.(some int) None @@
  Arg.info
    ~doc:"The uid of the user that will be used to build things"
    ~docv:"UID"
    ["uid"]
  
let fallback_library_path =
  Arg.required @@
  Arg.opt Arg.(some file) None @@
  Arg.info
    ~doc:"The fallback path of the dynamic libraries"
    ~docv:"FALLBACK"
    ["fallback"]

let scoreboard =
  Arg.required @@
  Arg.opt Arg.(some file) None @@
  Arg.info
    ~doc:"The scoreboard directory for user/homdir symlinks"
    ~docv:"SCOREBOARD"
    ["scoreboard"]

let cmdliner : config Term.t = 
  let make uid fallback_library_path scoreboard = 
    {uid; fallback_library_path; scoreboard}
  in
  Term.(const make $ uid $ fallback_library_path $ scoreboard)
