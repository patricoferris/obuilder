open Lwt.Infix 

(* XXX @patricoferris: Much has been copied from runc as I try to 
   customise it for the niche macos case... *)

type t = {
  macos_user : string;
  (* What user to run a build in -- the "sandbox" *)
}

(* Run the commands as the user, the cwd will be /Users/$USER ie. ~/ *)
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


let run ~cancelled ?stdin:stdin ~log t config _results_dir =
  Os.with_pipe_from_child @@ fun ~r:out_r ~w:out_w ->
  let cmd = run_as ~cwd:config.Config.cwd ~user:t.macos_user ~cmd:config.Config.argv in
  let stdout = `FD_copy out_w.raw in
  let stderr = stdout in
  let copy_log = copy_to_log ~src:out_r ~dst:log in
  let proc =
    let stdin = Option.map (fun x -> `FD_copy x.Os.raw) stdin in
    let pp f = Os.pp_cmd f config.argv in
    Os.exec_result ?stdin ~stdout ~stderr ~pp cmd
  in
  Os.close out_w;
  Option.iter Os.close stdin;
  Lwt.on_termination cancelled (fun () ->
      let rec aux () =
        if Lwt.is_sleeping proc then (
          (* XXX @patricoferris: What is the equivalent killing here...*)
          Lwt.return @@ Ok (Log.info (fun f -> f "MacOS Kill"))
           >>= function
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
  copy_log >>= fun () ->
  if Lwt.is_sleeping cancelled then Lwt.return (r :> (unit, [`Msg of string | `Cancelled]) result)
  else Lwt_result.fail `Cancelled

let create macos_user = 
  { macos_user }

let sandbox = `Macos 