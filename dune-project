(lang dune 3.4)

(name asli)

(generate_opam_files true)

(source
 (github HadrienRenaud/aslinterp))

(authors "Hadrien Renaud")

(maintainers "Hadrien Renaud")

(license LICENSE)

(documentation https://url/to/documentation)

(using menhir 2.1)

(package
 (name asli)
 (synopsis "A small but functionnal interpretor for ASL")
 (depends ocaml dune zarith alcotest menhir)
 (tags
  (topics asl ocaml interpretor)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
