open Lwt.Infix

let ( / ) = Filename.concat

type t = {
  runc_state_dir : string;
  fast_sync : bool;
  arches : string list;
}

let version = "runc-sandboxing"

module Saved_context = struct
  type t = {
    env : Os.env;
  } [@@deriving sexp]
end

let get_machine () =
  let ch = Unix.open_process_in "uname -m" in
  let arch = input_line ch in
  match Unix.close_process_in ch with
  | Unix.WEXITED 0 -> String.trim arch
  | _ -> failwith "Failed to get arch with 'uname -m'"

let get_arches () =
  if Sys.unix then (
    match get_machine () with
    | "x86_64" -> ["SCMP_ARCH_X86_64"; "SCMP_ARCH_X86"; "SCMP_ARCH_X32"]
    | "aarch64" -> ["SCMP_ARCH_AARCH64"; "SCMP_ARCH_ARM"]
    | _ -> []
  ) else (
    []
  )

module Json_config = struct
  let mount ?(options=[]) ~ty ~src dst =
    `Assoc [
      "destination", `String dst;
      "type", `String ty;
      "source", `String src;
      "options", `List (List.map (fun x -> `String x) options);
    ]

  let user_mounts =
    List.map @@ fun { Config.Mount.src; dst } ->
    mount ~ty:"bind" ~src dst
          ~options:[
            "bind";
            "nosuid";
            "nodev";
          ]

  let strings xs = `List ( List.map (fun x -> `String x) xs)

  let namespace x = `Assoc [ "type", `String x ]

  (* This is a subset of the capabilities that Docker uses by default.
     These control what root can do in the container.
     If the init process is non-root, permitted, effective and ambient sets are cleared.
     See capabilities(7) for full details. *)
  let default_linux_caps = [
    "CAP_CHOWN";                (* Make arbitrary changes to file UIDs and GIDs *)
    "CAP_DAC_OVERRIDE";         (* Bypass file read, write, and execute permission checks. *)
    "CAP_FSETID";               (* Set SUID/SGID bits. *)
    "CAP_FOWNER";               (* Bypass permission checks. *)
    "CAP_MKNOD";                (* Create special files using mknod. *)
    "CAP_SETGID";               (* Make arbitrary manipulations of process GIDs. *)
    "CAP_SETUID";               (* Make arbitrary manipulations of process UIDs. *)
    "CAP_SETFCAP";              (* Set arbitrary capabilities on a file. *)
    "CAP_SETPCAP";              (* Add any capability from bounding set to inheritable set. *)
    "CAP_SYS_CHROOT";           (* Use chroot. *)
    "CAP_KILL";                 (* Bypass permission checks for sending signals. *)
    "CAP_AUDIT_WRITE"           (* Write records to kernel auditing log. *)
    (* Allowed by Docker, but disabled here (because we use host networking):
    "CAP_NET_RAW";              (* Use RAW and PACKET sockets / bind to any address *)
    "CAP_NET_BIND_SERVICE";     (* Bind a socket to Internet domain privileged ports. *)
    *)
  ]

  let seccomp_syscalls ~fast_sync =
    if fast_sync then [
      `Assoc [
        (* Sync calls are pointless for the builder, because if the computer crashes then we'll
           just throw the build dir away and start again. And btrfs sync is really slow.
           Based on https://bblank.thinkmo.de/using-seccomp-to-filter-sync-operations.html
           Note: requires runc >= v1.0.0-rc92. *)
        "names", strings [
          "fsync";
          "fdatasync";
          "msync";
          "sync";
          "syncfs";
          "sync_file_range";
        ];
        "action", `String "SCMP_ACT_ERRNO";
        "errnoRet", `Int 0;                         (* Return error "success" *)
      ];
    ] else [
    ]

  let seccomp_policy t =
    let fields = [
      "defaultAction", `String "SCMP_ACT_ALLOW";
      "syscalls", `List (seccomp_syscalls ~fast_sync:t.fast_sync);
    ] @ (if t.arches = [] then [] else ["architectures", strings t.arches])
    in
    `Assoc fields

  let make {Config.cwd; argv; hostname; user; env; mounts; network} t ~config_dir ~results_dir : Yojson.Safe.t =
    let user =
      let { Obuilder_spec.uid; gid } = user in
      `Assoc [
        "uid", `Int uid;
        "gid", `Int gid;
      ]
    in
    let network_ns =
      match network with
      | ["host"] -> []
      | [] -> ["network"]
      | xs -> Fmt.failwith "Unsupported network configuration %a" Fmt.Dump.(list string) xs
    in
    let namespaces = network_ns @ ["pid"; "ipc"; "uts"; "mount"] in
    `Assoc [
      "ociVersion", `String "1.0.1-dev";
      "process", `Assoc [
        "terminal", `Bool false;
        "user", user;
        "args", strings argv;
        "env", strings (List.map (fun (k, v)  -> Printf.sprintf "%s=%s" k v) env);
        "cwd", `String cwd;
        "capabilities", `Assoc [
          "bounding", strings default_linux_caps;       (* Limits capabilities gained on execve. *)
          "effective", strings default_linux_caps;      (* Checked by kernel to decide access *)
          "inheritable", strings default_linux_caps;    (* Preserved across an execve (if root, or cap in ambient set) *)
          "permitted", strings default_linux_caps;      (* Limiting superset for the effective capabilities *)
        ];
        "rlimits", `List [
          `Assoc [
            "type", `String "RLIMIT_NOFILE";
            "hard", `Int 1024;
            "soft", `Int 1024
          ];
        ];
        "noNewPrivileges", `Bool false;
      ];
      "root", `Assoc [
        "path", `String (results_dir / "rootfs");
        "readonly", `Bool false;
      ];
      "hostname", `String hostname;
      "mounts", `List (
        mount "/proc"
          ~options:[      (* TODO: copy to others? *)
            "nosuid";
            "noexec";
            "nodev";
          ]
          ~ty:"proc"
          ~src:"proc" ::
        mount "/dev"
          ~ty:"tmpfs"
          ~src:"tmpfs"
          ~options:[
            "nosuid";
            "strictatime";
            "mode=755";
            "size=65536k";
          ] ::
        mount "/dev/pts"
          ~ty:"devpts"
          ~src:"devpts"
          ~options:[
            "nosuid";
            "noexec";
            "newinstance";
            "ptmxmode=0666";
            "mode=0620";
            "gid=5";            (* tty *)
          ] ::
        mount "/sys"            (* This is how Docker does it. runc's default is a bit different. *)
          ~ty:"sysfs"
          ~src:"sysfs"
          ~options:[
            "nosuid";
            "noexec";
            "nodev";
            "ro";
          ] ::
        mount "/sys/fs/cgroup"
          ~ty:"cgroup"
          ~src:"cgroup"
          ~options:[
            "ro";
            "nosuid";
            "noexec";
            "nodev";
          ] ::
        mount "/dev/shm"
          ~ty:"tmpfs"
          ~src:"shm"
          ~options:[
            "nosuid";
            "noexec";
            "nodev";
            "mode=1777";
            "size=65536k";
          ] ::
        mount "/dev/mqueue"
          ~ty:"mqueue"
          ~src:"mqueue"
          ~options:[
            "nosuid";
            "noexec";
            "nodev";
          ] ::
        mount "/etc/hosts"
          ~ty:"bind"
          ~src:(config_dir / "hosts")
          ~options:[
            "rbind";
            "rprivate"
          ] ::
        (if network = ["host"] then
           [ mount "/etc/resolv.conf"
               ~ty:"bind"
               ~src:"/etc/resolv.conf"
               ~options:[
                 "rbind";
                 "rprivate"
               ]
           ]
         else []
        ) @
        user_mounts mounts
      );
      "linux", `Assoc [
        "namespaces", `List (List.map namespace namespaces);
        "maskedPaths", strings [
          "/proc/acpi";
          "/proc/asound";
          "/proc/kcore";
          "/proc/keys";
          "/proc/latency_stats";
          "/proc/timer_list";
          "/proc/timer_stats";
          "/proc/sched_debug";
          "/sys/firmware";
          "/proc/scsi"
        ];
        "readonlyPaths", strings [
          "/proc/bus";
          "/proc/fs";
          "/proc/irq";
          "/proc/sys";
          "/proc/sysrq-trigger"
        ];
        "seccomp", seccomp_policy t;
      ];
    ]
end  

let next_id = ref 0

let copy_to_log ~src ~dst =
  let buf = Bytes.create 4096 in
  let rec aux () =
    Lwt_unix.read src buf 0 (Bytes.length buf) >>= function
    | 0 -> Lwt.return_unit
    | n -> Build_log.write dst (Bytes.sub_string buf 0 n) >>= aux
  in
  aux ()

let with_container ~log base fn =
  Os.with_pipe_from_child (fun ~r ~w ->
      (* We might need to do a pull here, so log the output to show progress. *)
      let copy = copy_to_log ~src:r ~dst:log in
      Os.pread ~stderr:(`FD_move_safely w) ["docker"; "create"; "--"; base] >>= fun cid ->
      copy >|= fun () ->
      String.trim cid
    ) >>= fun cid ->
  Lwt.finalize
    (fun () -> fn cid)
    (fun () -> Os.exec ["docker"; "rm"; "--"; cid])

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

let from ~log ~from_stage _t =
  let base = snd from_stage in 
  log `Heading (Fmt.strf "(from %a)" Sexplib.Sexp.pp_hum (Atom base));
  (fun ~cancelled:_ ~log tmp ->
      Log.info (fun f -> f "Base image not present; importing %S...@." base);
      let rootfs = tmp / "rootfs" in
      Os.sudo ["mkdir"; "--mode=755"; "--"; rootfs] >>= fun () ->
      (* Lwt_process.exec ("", [| "docker"; "pull"; "--"; base |]) >>= fun _ -> *)
      with_container ~log base (fun cid ->
          Os.with_pipe_between_children @@ fun ~r ~w ->
          let exporter = Os.exec ~stdout:(`FD_move_safely w) ["docker"; "export"; "--"; cid] in
          let tar = Os.sudo ~stdin:(`FD_move_safely r) ["tar"; "-C"; rootfs; "-xf"; "-"] in
          exporter >>= fun () ->
          tar
        ) >>= fun () ->
      export_env base >>= fun env ->
      Os.write_file ~path:(tmp / "env")
        (Sexplib.Sexp.to_string_hum Saved_context.(sexp_of_t {env})) >>= fun () ->
      Lwt_result.return ()
    )

let run ~cancelled ?stdin:stdin ~log t config results_dir =
  Lwt_io.with_temp_dir ~prefix:"obuilder-runc-" @@ fun tmp ->
  let json_config = Json_config.make config ~config_dir:tmp ~results_dir t in
  Os.write_file ~path:(tmp / "config.json") (Yojson.Safe.pretty_to_string json_config ^ "\n") >>= fun () ->
  Os.write_file ~path:(tmp / "hosts") "127.0.0.1 localhost builder" >>= fun () ->
  let id = string_of_int !next_id in
  incr next_id;
  Os.with_pipe_from_child @@ fun ~r:out_r ~w:out_w ->
  let cmd = ["runc"; "--root"; t.runc_state_dir; "run"; id] in
  let stdout = `FD_move_safely out_w in
  let stderr = stdout in
  let copy_log = copy_to_log ~src:out_r ~dst:log in
  let proc =
    let stdin = Option.map (fun x -> `FD_move_safely x) stdin in
    let pp f = Os.pp_cmd f config.argv in
    Os.sudo_result ~cwd:tmp ?stdin ~stdout ~stderr ~pp cmd
  in
  Lwt.on_termination cancelled (fun () ->
      let rec aux () =
        if Lwt.is_sleeping proc then (
          let pp f = Fmt.pf f "runc kill %S" id in
          Os.sudo_result ~cwd:tmp ["runc"; "--root"; t.runc_state_dir; "kill"; id; "KILL"] ~pp >>= function
          | Ok () -> Lwt.return_unit
          | Error (`Msg m) ->
            (* This might be because it hasn't been created yet, so retry. *)
            Log.warn (fun f -> f "kill failed: %s (will retry in 10s)" m);
            Lwt_unix.sleep 10.0 >>= aux
        ) else Lwt.return_unit  (* Process has already finished *)
      in
      Lwt.async aux
    );
  proc >>= fun r ->
  copy_log >>= fun () ->
  if Lwt.is_sleeping cancelled then Lwt.return (r :> (unit, [`Msg of string | `Cancelled]) result)
  else Lwt_result.fail `Cancelled

let clean_runc dir =
  Sys.readdir dir
  |> Array.to_list
  |> Lwt_list.iter_s (fun item ->
      Log.warn (fun f -> f "Removing left-over runc container %S" item);
      Os.sudo ["runc"; "--root"; dir; "delete"; item]
    )

let create ?(fast_sync=false) ~runc_state_dir () =
  Os.ensure_dir runc_state_dir;
  let arches = get_arches () in
  Log.info (fun f -> f "Architectures for multi-arch system: %a" Fmt.(Dump.list string) arches);
  clean_runc runc_state_dir >|= fun () ->
  { runc_state_dir; fast_sync; arches }
open Sexplib.Conv 

type config = {
  fast_sync : bool;
} [@@deriving sexp]

let create ?state_dir (c : config) =
  match state_dir with 
    | None -> Fmt.failwith "Runc requires a state directory"
    | Some runc_state_dir -> 
        Os.ensure_dir runc_state_dir;
        let arches = get_arches () in
        Log.info (fun f -> f "Architectures for multi-arch system: %a" Fmt.(Dump.list string) arches);
        { runc_state_dir; fast_sync = c.fast_sync; arches }

let clean _ = Lwt.return ()

open Cmdliner 

let fast_sync =
  Arg.value @@
  Arg.opt Arg.bool false @@
  Arg.info
    ~doc:"Install a seccomp filter that skips allsync syscalls"
    ~docv:"FAST_SYNC"
    ["fast-sync"]

let cmdliner : config Term.t = 
  let make fast_sync = 
    { fast_sync }
  in
  Term.(const make $ fast_sync)