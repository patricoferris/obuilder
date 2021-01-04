include S.STORE

val create : prefix:string option -> pool:string -> t Lwt.t
(** [create ~prefix ~pool] is a new store in zfs pool [pool] using [prefix] as the 
    top-level directory i.e. "/<prefix>/<pool>" it defaults to [/] when [None] is given. *)
