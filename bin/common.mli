open Cmdliner 

type builder = Builder : (module Obuilder.BUILDER with type t = 'a) * 'a -> builder

val spec_file : string Term.t 

val store : Obuilder.Store_spec.t Term.t 