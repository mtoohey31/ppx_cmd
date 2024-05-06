(lang dune 3.11)

(name ppx_cmd)

(generate_opam_files true)

(source
 (github mtoohey31/ppx_cmd))

(authors "Matthew Toohey <contact@mtoohey.com>")

(maintainers "Matthew Toohey <contact@mtoohey.com>")

(license BSD-3-Clause)

(package
 (name ppx_cmd)
 (synopsis "[@@deriving cmd]")
 (depends (ocaml (>= 4.11)) dune))
