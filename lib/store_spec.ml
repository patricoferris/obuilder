open Lwt.Infix

type t = [
  | `Btrfs of string                  (* Path *)
  | `Zfs of string option * string    (* Prefix & Pool *)
]

let of_string s =
  match Astring.String.cut s ~sep:":" with
  | Some ("zfs", pool) -> begin
    match Astring.String.cut pool ~sep:"/" with 
    | Some (prefix, pool) -> Ok (`Zfs (Some prefix, pool))
    | None -> Ok (`Zfs (None, pool))
  end
  | Some ("btrfs", path) -> Ok (`Btrfs path)
  | _ -> Error (`Msg "Store must start with zfs: or btrfs:")

let pp f = function
  | `Zfs (_, pool) -> Fmt.pf f "zfs:%s" pool
  | `Btrfs path -> Fmt.pf f "btrfs:%s" path

type store = Store : (module S.STORE with type t = 'a) * 'a -> store

let to_store = function
  | `Btrfs path ->
    Btrfs_store.create path >|= fun store ->
    Store ((module Btrfs_store), store)
  | `Zfs (prefix, pool) ->
    Zfs_store.create ~prefix ~pool >|= fun store ->
    Store ((module Zfs_store), store)
