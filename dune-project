(lang dune 3.16)

(name huecaml)

(generate_opam_files true)

(source
 (github j-simian/huecaml))

(authors "Jessica Simian <me@jsimian.com>")

(maintainers "Jessica Simian <me@jsimian.com>")

(license MIT)

(package
 (name huecaml)
 (synopsis "An OCaml interface to the Philip's Hue API")
 (description "An OCaml interface to the Philip's Hue API")
 (depends
  (ocaml (>= 5.0))
  (async (>= 0.17))
  (core (>= 0.17))
  (cohttp-async (>= 5.0))
  (jsonaf (>= 0.17))
  (ppx_jsonaf_conv (>= 0.17))
  (uri (>= 4.0)))
 (tags ()))
