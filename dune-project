(lang dune 2.8)

(generate_opam_files true)

(name lib1)

(package
 (name lib1)
 (depends
  (inline_tests :with-test)))
