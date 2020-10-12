open Workflow 

type job = { job : Types.job; }

type t = job Types.t 

module Conf = Conf

val job_to_yaml : job -> Yaml.value 
val job_of_yaml : Yaml.value -> job

val workflow_of_spec : 
    ?oses:string list ->
    ?ovs:string list -> 
    ?use_docker:bool -> 
    Spec.stage -> 
    job Types.t
(** [workflow_of_spec x] produces a Github Action workflow that aims to be equivalent to [x].
    Experimental. *)

val pp : Format.formatter -> t -> unit 
(** Pretty printer *)

val to_string : t -> string 
(** [to_string t] prints the workflow to a string *)