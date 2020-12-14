open Cmdliner
open Sexplib.Conv

type t = {
  dir : string;
  expect :
    (cancelled:unit Lwt.t ->
     ?stdin:Obuilder.Os.unix_fd ->
     log:Obuilder.Build_log.t ->
     Obuilder.Config.t ->
     string ->
     (unit, [`Msg of string | `Cancelled]) Lwt_result.t) Queue.t;
}

type config = {
  dir : string;
}[@@deriving sexp]


let dir =
  Arg.required @@
  Arg.opt Arg.(some file) None @@
  Arg.info
    ~doc:"Directory"
    ~docv:"DIR"
    ["dir"]

let cmdliner : config Term.t = 
  let make dir = 
    { dir }
  in
  Term.(const make $ dir)

let fake_config = { dir = "" }

let version = "mock-sandbox"

let expect t x = Queue.add x t.expect

let from ~log:_ ~from_stage:_ _t = 
  fun ~cancelled:_ ~log:_ _ -> Lwt.return (Ok () :> (unit, [ `Cancelled | `Msg of string ]) result)

let run ~cancelled ?stdin ~log t (config:Obuilder.Config.t) dir =
  match Queue.take_opt t.expect with
  | None -> Fmt.failwith "Unexpected sandbox execution: %a" Fmt.(Dump.list string) config.argv
  | Some fn ->
    Lwt.catch
      (fun () -> fn ~cancelled ?stdin ~log config dir)
      (function
        | Failure ex -> Lwt_result.fail (`Msg ex)
        | ex -> Lwt_result.fail (`Msg (Printexc.to_string ex))
      )

let create ?state_dir:dir conf = 
  match dir with 
    | Some dir -> { dir; expect = Queue.create () }
    | None -> {dir = conf.dir; expect = Queue.create ()} 
