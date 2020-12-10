open Lwt.Infix

let () =
  Logs.set_reporter (Logs_fmt.reporter ())

let ( / ) = Filename.concat

type builder = Builder : (module Obuilder.BUILDER with type t = 'a) * 'a -> builder

let log tag msg =
  match tag with
  | `Heading -> Fmt.pr "%a@." Fmt.(styled (`Fg (`Hi `Blue)) string) msg
  | `Note -> Fmt.pr "%a@." Fmt.(styled (`Fg `Yellow) string) msg
  | `Output -> output_string stdout msg; flush stdout 

let runc fast_sync store spec src_dir =
  Lwt_main.run begin
    Linux.create_builder ~fast_sync ~spec:store () >>= fun (Builder ((module Builder), builder)) ->
    let spec = Obuilder.Spec.t_of_sexp (Sexplib.Sexp.load_sexp spec) in
    let context = Obuilder.Context.v ~log ~src_dir () in
    Builder.build builder context spec >>= function
    | Ok x ->
      Fmt.pr "Got: %S@." (x :> string);
      Lwt.return_unit
    | Error `Cancelled ->
      Fmt.epr "Cancelled at user's request@.";
      exit 1
    | Error (`Msg m) ->
      Fmt.epr "Build step failed: %s@." m;
      exit 1
  end

let macos store spec src_dir uid fallback_library_path scoreboard =
  Lwt_main.run begin
    Macos.create_builder ~spec:store ~uid ~fallback_library_path ~scoreboard >>= fun (Builder ((module Builder), builder)) ->
    let spec = Obuilder.Spec.t_of_sexp (Sexplib.Sexp.load_sexp spec) in
    let context = Obuilder.Context.v ~log ~src_dir () in
    Builder.build builder context spec >>= function
    | Ok x ->
      Fmt.pr "Got: %S@." (x :> string);
      Lwt.return_unit
    | Error `Cancelled ->
      Fmt.epr "Cancelled at user's request@.";
      exit 1
    | Error (`Msg m) ->
      Fmt.epr "Build step failed: %s@." m;
      exit 1
  end

let healthcheck fast_sync verbose store =
  if verbose then
    Logs.Src.set_level Obuilder.log_src (Some Logs.Info);
  Lwt_main.run begin
    Linux.create_builder ?fast_sync ~spec:store () >>= fun (Builder ((module Builder), builder)) ->
    Builder.healthcheck builder >|= function
    | Error (`Msg m) ->
      Fmt.epr "Healthcheck failed: %s@." m;
      exit 1
    | Ok () ->
      Fmt.pr "Healthcheck passed@."
  end

let delete store id =
  Lwt_main.run begin
    Linux.create_builder ~spec:store () >>= fun (Builder ((module Builder), builder)) ->
    Builder.delete builder id ~log:(fun id -> Fmt.pr "Removing %s@." id)
  end

let dockerfile buildkit spec =
  Sexplib.Sexp.load_sexp spec
  |> Obuilder_spec.t_of_sexp
  |> Obuilder_spec.Docker.dockerfile_of_spec ~buildkit
  |> print_endline

open Cmdliner

let src_dir =
  Arg.required @@
  Arg.pos 0 Arg.(some dir) None @@
  Arg.info
    ~doc:"Directory containing the source files"
    ~docv:"DIR"
    []

let id =
  Arg.required @@
  Arg.pos 0 Arg.(some string) None @@
  Arg.info
    ~doc:"The ID of a build within the store"
    ~docv:"ID"
    []

let runc =
  let doc = "Build a spec file using runc." in
  Term.(const runc $ Linux.fast_sync $ Common.store $ Common.spec_file $ src_dir),
  Term.info "runc" ~doc

let macos =
  let doc = "Build a spec file using the macos setup." in
  Term.(const macos $ Common.store $ Common.spec_file $ src_dir $ Macos.uid $ Macos.fallback_library_path $ Macos.scoreboard),
  Term.info "macos" ~doc

let delete =
  let doc = "Recursively delete a cached build result." in
  Term.(const delete $ Common.store $ id),
  Term.info "delete" ~doc

let buildkit =
  Arg.value @@
  Arg.flag @@
  Arg.info
    ~doc:"Output extended BuildKit syntax"
    ["buildkit"]

let dockerfile =
  let doc = "Convert a spec to Dockerfile format" in
  Term.(const dockerfile $ buildkit $ Common.spec_file),
  Term.info "dockerfile" ~doc

let cmds = [runc; macos; delete; dockerfile]

let default_cmd =
  let doc = "a command-line interface for OBuilder" in
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info "obuilder" ~doc

let term_exit (x : unit Term.result) = Term.exit x

let () =
  Logs.(set_level (Some Debug));
  Fmt_tty.setup_std_outputs ();
  term_exit @@ Term.eval_choice default_cmd cmds