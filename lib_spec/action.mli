open Workflow 

type job

type t = job Types.t 

val workflow_of_spec : Spec.stage -> t
(** [workflow_of_spec x] produces a Github Action workflow that aims to be equivalent to [x].

    Experimental. *)

val to_string : t -> string 
(** [to_string t] prints the workflow to a string *)