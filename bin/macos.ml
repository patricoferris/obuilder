open Lwt 

module Macos = Obuilder.Macos_sandbox

let create_builder ~spec ~uid ~fallback_library_path ~scoreboard =
  Obuilder.Store_spec.to_store spec >|= fun (Store ((module Store), store)) -> 
  let module Builder = Obuilder.Builder(Store)(Macos) in
  let sandbox = Macos.create ~uid ~fallback_library_path ~scoreboard in
  let builder = Builder.v ~store ~sandbox in
  Common.Builder ((module Builder), builder)

(* MacOS specific flags *)
open Cmdliner 
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

