; This script builds OBuilder itself using a snapshot of the ocurrent/opam:debian-10-4.11 base image.
;
; Run it from the top-level of the OBuilder source tree, e.g.
;
;   dune exec -- obuilder build --store=zfs:tank -f example.spec .
;
; The result can then be found in /tank/HASH/rootfs/ (where HASH is displayed at the end of the build).

((from ("macos" "4.11.0"))
 (env PATH "/Users/patrickferris/ocaml/4.11.0/bin")
 (run (shell "env")))