; This script builds OBuilder itself using a snapshot of the ocurrent/opam:debian-10-4.11 base image.
;
; Run it from the top-level of the OBuilder source tree, e.g.
;
;   dune exec -- obuilder build --store=zfs:tank -f example.spec .
;
; The result can then be found in /tank/HASH/rootfs/ (where HASH is displayed at the end of the build).

((from ("macos" "4.11.0"))
 (run (shell "opam init"))
 (run (shell "opam source ezjsonm.1.2.0"))
 (workdir /ezjsonm.1.2.0)
 (run (shell "opam pin add -yn ."))
 (run (shell "opam depext -y ezjsonm ezjsonm-lwt"))
 (copy (src sandbox.sh) (dst ./.opam/opam-init/hooks/sandbox.sh))
 (run (shell "opam install --deps-only -t -y ezjsonm && opam install --deps-only -t -y ezjsonm-lwt"))
 (run (shell "opam exec -- dune build @install @runtest && rm -rf _build")))