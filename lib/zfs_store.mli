include S.STORE

val create : prefix:string option -> pool:string -> t Lwt.t
(** [create ~prefix ~pool ()] is a new store in zfs pool [pool] using [prefix]
    as the top level directory of the file system e.g. "/<prefix>/tank", it 
    defaults to [/] of [None] is supplied *)
