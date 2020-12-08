open Cmdliner

val create_builder : 
  spec:Obuilder.Store_spec.t -> 
  uid:int -> 
  fallback_library_path:string -> 
  Common.builder Lwt.t 

val uid : int Term.t 

val fallback_library_path : string Term.t 