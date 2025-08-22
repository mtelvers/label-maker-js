(lang dune 3.0)

(generate_opam_files true)

(package
 (name label-maker)
 (synopsis "Web-based PDF label maker for Avery formats")
 (description "A label maker application that generates PDF labels with custom text for various Avery label formats. Compiles to JavaScript for web deployment.")
 (maintainers "Mark Elvers <mark.elvers@tunbury.org>")
 (license MIT)
 (homepage "https://github.com/mtelvers/label-maker-js")
 (bug_reports "https://github.com/mtelvers/label-maker-js/issues")
 (source (github "mtelvers/label-maker-js"))
 (depends ocaml dune camlpdf otfm (js_of_ocaml (< "6.0.0")) js_of_ocaml-ppx vg))
