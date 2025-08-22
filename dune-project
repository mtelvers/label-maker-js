(lang dune 3.0)

(generate_opam_files true)

(package
 (name label-maker)
 (depends ocaml dune camlpdf otfm (js_of_ocaml (< "6.0.0")) js_of_ocaml-ppx vg))
