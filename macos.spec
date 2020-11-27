; Build a Macos Worker from template user (here Macos701), the default environment assumes
;
;  - Opam is already installed 
;  - So is `opam-repository`
;
;

((from user:macos701)
 (workdir /src)
 (env OPAM_HASH "3332c004db65ef784f67efdadc50982f000b718f") ; Fix the version of opam-repository we want
 (run 
  (network host)
  (shell
   "cd ~/opam-repository \
    && (git cat-file -e $OPAM_HASH || git fetch origin master) \
    && git reset -q --hard $OPAM_HASH \
    && git log --no-decorate -n1 --oneline \
    && opam update -u"))
 (copy (src obuilder-spec.opam obuilder.opam) (dst ./))                     ; Copy just the opam file first (helps caching)
 (run (shell "opam pin add -yn ."))
 ; Install OS package dependencies
 (run
  (network host)
  (cache (opam-archives (target ~/.opam/download-cache)))
  (shell "opam depext -y obuilder"))
 ; Install OCaml dependencies
 (run
  (network host)
  (cache (opam-archives (target ~/.opam/download-cache)))
  (shell "opam install --deps-only -t obuilder"))
 (copy                                                  ; Copy the rest of the source code
  (src .)
  (dst /src/)
  (exclude .git _build))
 (run (shell "opam exec -- dune build @install @runtest && rm -rf _build")))    ; Build and test
