open Obuilder 
open Cmdliner 

val create_builder : 
  ?fast_sync:bool ->
  spec:Store_spec.t -> 
  unit -> 
  Common.builder Lwt.t 

val fast_sync : bool Term.t 