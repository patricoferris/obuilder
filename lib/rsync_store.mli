(** Store build results as using rsync. *)

include S.STORE

val create : path:string -> t Lwt.t
(** [create ~path] creates a new rsync store where everything will
    store under [path]. *)
