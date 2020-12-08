open Lwt 

module Runc = Obuilder.Runc_sandbox

let create_builder ?fast_sync ~spec () =
  let ( / ) = Filename.concat in 
  Obuilder.Store_spec.to_store spec >>= fun (Store ((module Store), store)) -> 
  let module Builder = Obuilder.Builder(Store)(Runc) in
  Runc.create ?fast_sync ~runc_state_dir:(Store.state_dir store / "runc") () >|= fun sandbox -> 
  let builder = Builder.v ~store ~sandbox in
  Common.Builder ((module Builder), builder)

open Cmdliner
let fast_sync =
  Arg.value @@
  Arg.flag @@
  Arg.info
    ~doc:"Ignore sync syscalls (requires runc >= 1.0.0-rc92)"
    ["fast-sync"]