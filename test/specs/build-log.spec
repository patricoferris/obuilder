; This build-log spec file purposefully has a failing
; run command to test how real sandboxes handle a failure
; and in particular how the build log is handles with 
; the destruction of the result directory.

; See this PR for more details: https://github.com/ocurrent/obuilder/pull/98

((from ocaml/opam@sha256:af617095b1255a61f82ac1873031a850dd29c37865ff5bf691ab5ccd29187679)
 (run (shell "exit 1")))
