open Lwt.Infix
open Sexplib.Std

let ( / ) = Filename.concat
let ( >>!= ) = Lwt_result.bind

let hostname = "builder"

module Context = struct
  type t = {
    switch : Lwt_switch.t option;
    env : Os.env;                       (* Environment in which to run commands. *)
    src_dir : string;                   (* Directory with files for copying. *)
    user : Obuilder_spec.user;                   (* Container user to run as. *)
    workdir : string;                   (* Directory in the container namespace for cwd. *)
    shell : string list;
    log : S.logger;
  }

  let v ?switch ?(env=[]) ?(user=Obuilder_spec.root) ?(workdir="/") ?(shell=["/bin/bash"; "-c"]) ~log ~src_dir () =
    { switch; env; src_dir; user; workdir; shell; log }
end

module Saved_context = struct
  type t = {
    env : Os.env;
  } [@@deriving sexp]
end

module Make (Raw_store : S.STORE) (Sandbox : S.SANDBOX) = struct
  module Store = Db_store.Make(Raw_store)

  type t = {
    store : Store.t;
    sandbox : Sandbox.t;
  }

  (* Inputs to run that should affect the hash. i.e. if anything in here changes
     then we need a fresh build. *)
  type run_input = {
    base : S.id;
    workdir : string;
    user : Obuilder_spec.user;
    env : Os.env;
    cmd : string;
    shell : string list;
    network : string list;
  } [@@deriving sexp_of]

  let run t ~switch ~log ~cache run_input =
    let id =
      sexp_of_run_input run_input
      |> Sexplib.Sexp.to_string_mach
      |> Sha256.string
      |> Sha256.to_hex
    in
    let { base; workdir; user; env; cmd; shell; network } = run_input in
    Store.build t.store ?switch ~base ~id ~log (fun ~cancelled ~log result_tmp ->
        let to_release = ref [] in
        Lwt.finalize
          (fun () ->
             cache |> Lwt_list.map_s (fun { Obuilder_spec.Cache.id; target; buildkit_options = _ } ->
                 Store.cache ~user t.store id >|= fun (src, release) ->
                 to_release := release :: !to_release;
                 { Config.Mount.src; dst = target }
               )
             >>= fun mounts ->
             let argv = shell @ [cmd] in
             let config = Config.v ~cwd:workdir ~argv ~hostname ~user ~env ~mounts ~network in
             Os.with_pipe_to_child @@ fun ~r:stdin ~w:close_me ->
             Lwt_unix.close close_me >>= fun () ->
             Sandbox.run ~cancelled ~stdin ~log t.sandbox config result_tmp
          )
          (fun () ->
             !to_release |> Lwt_list.iter_s (fun f -> f ())
          )
      )

  type copy_details = {
    base : S.id;
    user : Obuilder_spec.user;
    op : [`Copy_items of Manifest.t list * string | `Copy_item of Manifest.t * string];
  } [@@deriving sexp_of]

  let rec sequence = function
    | [] -> Ok []
    | Error e :: _ -> Error e
    | Ok x :: xs ->
      match sequence xs with
      | Ok xs -> Ok (x :: xs)
      | e -> e

  let to_copy_op ~dst = function
    | [] -> Fmt.error_msg "No source items for copy!"
    | items when dst.[String.length dst - 1] = '/' -> Ok (`Copy_items (items, dst))
    | [item] -> Ok (`Copy_item (item, dst))
    | _ -> Fmt.error_msg "When copying multiple items, the destination must end with '/'"

  let mac_copy t ~context ~base src dst exclude = 
    let { Context.switch; src_dir; workdir; user; log; shell = _; env = _ } = context in 
    let top_dir = if Sandbox.sandbox = `Macos then "/Users/mac703" else "" in
    let dst = if Filename.is_relative dst then top_dir / workdir / dst else top_dir / dst in 
    let src_manifest = sequence (List.map (Manifest.generate ~exclude ~src_dir) src) in
    let rec excludes acc = function 
      | [] -> acc 
      | x::xs -> excludes ("--exclude" :: x :: acc) xs
    in 
    match Result.bind src_manifest (to_copy_op ~dst) with
      | Error _ as e -> Lwt.return e
      | Ok op -> (
        let details = {
          base;
          op;
          user;
        } in
      let id = Sha256.to_hex (Sha256.string (Sexplib.Sexp.to_string (sexp_of_copy_details details))) in
      Store.build t.store ?switch ~base ~id ~log (fun ~cancelled ~log result_tmp ->
      let argv = "rsync" :: "-av" :: src @ [ dst ] @ (excludes [] exclude) in
      let config = Config.v
          ~cwd:"/"
          ~argv
          ~hostname
          ~user:Obuilder_spec.root
          ~env:["PATH", "/bin:/usr/bin"]
          ~mounts:[]
          ~network:[]
      in
      Os.with_pipe_to_child @@ fun ~r:from_us ~w:_to_untar ->
      let proc = Sandbox.run ~cancelled ~stdin:from_us ~log t.sandbox config result_tmp in
      proc >>= fun result ->
      Lwt.return result
      ))
  let copy t ~context ~base { Obuilder_spec.src; dst; exclude } =
    match Sandbox.sandbox with 
      | `Runc -> (
    let { Context.switch; src_dir; workdir; user; log; shell = _; env = _ } = context in
    let dst = if Filename.is_relative dst then workdir / dst else dst in
    let src_manifest = sequence (List.map (Manifest.generate ~exclude ~src_dir) src) in
    match Result.bind src_manifest (to_copy_op ~dst) with
    | Error _ as e -> Lwt.return e
    | Ok op -> (
      let details = {
        base;
        op;
        user;
      } in
      (* Fmt.pr "COPY: %a@." Sexplib.Sexp.pp_hum (sexp_of_copy_details details); *)
      let id = Sha256.to_hex (Sha256.string (Sexplib.Sexp.to_string (sexp_of_copy_details details))) in
      Store.build t.store ?switch ~base ~id ~log (fun ~cancelled ~log result_tmp ->
          let argv = ["tar"; "-xf"; "-"] in
          let config = Config.v
              ~cwd:"/"
              ~argv
              ~hostname
              ~user:Obuilder_spec.root
              ~env:["PATH", "/bin:/usr/bin"]
              ~mounts:[]
              ~network:[]
          in
          Os.with_pipe_to_child @@ fun ~r:from_us ~w:to_untar ->
          let proc = Sandbox.run ~cancelled ~stdin:from_us ~log t.sandbox config result_tmp in
          let send =
            (* If the sending thread finishes (or fails), close the writing socket
               immediately so that the tar process finishes too. *)
            Lwt.finalize
              (fun () ->
                 match op with
                 | `Copy_items (src_manifest, dst_dir) ->
                   Tar_transfer.send_files ~src_dir ~src_manifest ~dst_dir ~to_untar ~user
                 | `Copy_item (src_manifest, dst) ->
                   Tar_transfer.send_file ~src_dir ~src_manifest ~dst ~to_untar ~user
              )
              (fun () -> Lwt_unix.close to_untar)
          in
          proc >>= fun result ->
          send >>= fun () ->
          Lwt.return result
        )))
        | `Macos -> mac_copy t ~context ~base src dst exclude

  let pp_op ~(context:Context.t) f op =
    let sexp = Obuilder_spec.sexp_of_op op in
    Fmt.pf f "@[<v2>%s: %a@]" context.workdir Sexplib.Sexp.pp_hum sexp

  let update_workdir ~(context:Context.t) path =
    let workdir =
      if Astring.String.is_prefix ~affix:"/" path then path
      else context.workdir ^ "/" ^ path
    in
    { context with workdir }

  let rec run_steps t ~(context:Context.t) ~base = function
    | [] -> Lwt_result.return base
    | op :: ops ->
      context.log `Heading Fmt.(strf "%a" (pp_op ~context) op);
      let k = run_steps t ops in
      match op with
      | `Comment _ -> k ~base ~context
      | `Workdir workdir -> k ~base ~context:(update_workdir ~context workdir)
      | `User user -> k ~base ~context:{context with user}
      | `Run { shell = cmd; cache; network } ->
        let switch, run_input, log =
          let { Context.switch; workdir; user; env; shell; log; src_dir = _ } = context in
          (switch, { base; workdir; user; env; cmd; shell; network }, log)
        in
        run t ~switch ~log ~cache run_input >>!= fun base ->
        k ~base ~context
      | `Copy x ->
        copy t ~context ~base x >>!= fun base ->
        k ~base ~context
      | `Env ((key, _) as e) ->
        let env = e :: (List.remove_assoc key context.env) in
        k ~base ~context:{context with env}
      | `Shell shell ->
        k ~base ~context:{context with shell}

  let export_env base : Os.env Lwt.t =
    Os.pread ["docker"; "image"; "inspect";
              "--format"; {|{{range .Config.Env}}{{print . "\x00"}}{{end}}|};
              "--"; base] >|= fun env ->
    String.split_on_char '\x00' env
    |> List.filter_map (function
        | "\n" -> None
        | kv ->
          match Astring.String.cut ~sep:"=" kv with
          | None -> Fmt.failwith "Invalid environment in Docker image %S (should be 'K=V')" kv
          | Some _ as pair -> pair
      )

  let with_container base fn =
    Os.pread ["docker"; "create"; "--"; base] >>= fun cid ->
    let cid = String.trim cid in
    Lwt.finalize
      (fun () -> fn cid)
      (fun () -> Os.exec ["docker"; "rm"; "--"; cid])

  let get_base t ~log base =
    log `Heading (Fmt.strf "FROM %s" base);
    let id = Sha256.to_hex (Sha256.string base) in
    Store.build t.store ~id ~log (fun ~cancelled:_ ~log:_ tmp ->
        Log.info (fun f -> f "Base image not present; importing %S...@." base);
        let rootfs = tmp / "rootfs" in
        Os.sudo ["mkdir"; "--mode=755"; "--"; rootfs] >>= fun () ->
        (* Lwt_process.exec ("", [| "docker"; "pull"; "--"; base |]) >>= fun _ -> *)
        with_container base (fun cid ->
            let r, w = Unix.pipe ~cloexec:true () in
            let exporter, tar =
              Fun.protect
                (fun () ->
                   let exporter = Os.exec ~stdout:(`FD_copy w) ["docker"; "export"; "--"; cid] in
                   let tar = Os.sudo ~stdin:(`FD_copy r) ["tar"; "-C"; rootfs; "-xf"; "-"] in
                   exporter, tar
                )
                ~finally:(fun () ->
                    Unix.close r;
                    Unix.close w
                  )
            in
            exporter >>= fun () ->
            tar
          ) >>= fun () ->
        export_env base >>= fun env ->
        Os.write_file ~path:(tmp / "env")
          (Sexplib.Sexp.to_string_hum Saved_context.(sexp_of_t {env})) >>= fun () ->
        Lwt_result.return ()
      )
    >>!= fun id ->
    let path = Option.get (Store.result t.store id) in
    let { Saved_context.env } = Saved_context.t_of_sexp (Sexplib.Sexp.load_sexp (path / "env")) in
    Lwt_result.return (id, env)

  let get_user t ~log user = 
    log `Heading (Fmt.strf "FROM %s" user);
    let id = Sha256.to_hex (Sha256.string user) in
    Store.build t.store ~id ~log (fun ~cancelled:_ ~log:_ tmp ->
        Os.sudo ["rsync"; "-av"; "/Users/mac701/local/"; "/Users/mac703/local/"] >>= fun () -> 
        Os.sudo ["rsync"; "-av"; "/Users/mac701/Library/"; "/Users/mac703/Library/"] >>= fun () ->
        Os.sudo ["rsync"; "-av"; "/Users/mac701/tmp/"; "/Users/mac703/tmp/"] >>= fun () ->
        Os.sudo ["cp"; "/Users/mac701/.bash_profile"; "/Users/mac703/.bash_profile"] >>= fun () -> 
        Os.sudo ["chown"; "-R"; "mac703"; "/Users/mac703/"] >>= fun () -> 
        (* XXX patricoferris: Do I need to export the env too? *) 
        Lwt.return [] >>= fun env ->
        Os.write_file ~path:(tmp / "env")
          (Sexplib.Sexp.to_string_hum Saved_context.(sexp_of_t {env})) >>= fun () ->
        Lwt_result.return ()
      )
    >>!= fun id ->
    let _path = Option.get (Store.result t.store id) in
    (* let { Saved_context.env } = Saved_context.t_of_sexp (Sexplib.Sexp.load_sexp (path / "env")) in *)
    Lwt_result.return (id, [])

  let build t context { Obuilder_spec.from = base; ops } =
    match String.split_on_char ':' base with 
     | ["user"; user] -> 
      context.Context.log `Heading (Fmt.strf "FROM %s" user);
      (* Building for MacOS from a template user so rsync the results over *)
      get_user t ~log:context.Context.log user >>!= fun (id, env) ->
      let context = { context with env = context.env @ env } in
      run_steps t ~context ~base:id ops 
     | _ -> get_base t ~log:context.Context.log base >>!= fun (id, env) ->
      let context = { context with env = context.env @ env } in
      run_steps t ~context ~base:id ops

  let delete ?log t id =
    Store.delete ?log t.store id

  let prune ?log t ~before limit =
    Store.prune ?log t.store ~before limit

  let v ~store ~sandbox =
    let store = Store.wrap store in
    { store; sandbox }
end
