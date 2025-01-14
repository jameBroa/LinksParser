(lang dune 3.0)
(using menhir 2.0)
(name links)
(package
 (name links)
 (synopsis "The Links Programming Language")
 (description "Links is a functional programming language designed to make web programming easier.")
 (depends (ocaml (>= : 5.1.1))
          (dune-configurator (>= : 3.8))
           ppx_deriving
          (ppx_deriving_yojson (>= 3.3))
           base64
           linenoise
           ANSITerminal
          (lwt (>= 5.0.0))
           cohttp
           cohttp-lwt
           cohttp-lwt-unix
           conduit-lwt-unix
           uri
           tls
           websocket
           websocket-lwt-unix
           safepass
           result
           ocamlfind
          (menhir (>= 20210419))
          (ppx_sexp_conv (>= v0.16.0))
          (calendar (>= 2.0.4))
          (rdf_lwt (>= 0.13.0)))
 (tags
  ("web programming" "tierless" "multi-tier" "effect handlers" "effect typing" "session types"
   "concurrency" "webpage" "extensible data types" "language-integrated queries")))