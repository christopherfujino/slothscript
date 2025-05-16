(lang dune 3.16)

(name sloth_script)
(using menhir 2.1)

(generate_opam_files true)

(source
 (github christopherfujino/slothscript))

(authors "Author Name")

(maintainers "Maintainer Name")

(license "BSD-3-Clause")

(documentation https://url/to/documentation)

(package
 (name sloth_script)
 (synopsis "A slow scripting language")
 (description "A longer description")
 (depends ocaml dune ounit2)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
