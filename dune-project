(lang dune 3.16)

(name sloth_script)
(using menhir 2.1)

(generate_opam_files true)

(source
 (github christopherfujino/slothscript))

(authors "Christopher Fujino <christopherfujino@gmail.com>")

(maintainers "Christopher Fujino <christopherfujino@gmail.com>")

(license "BSD-3-Clause")

(documentation https://url/to/documentation)

(package
 (name sloth_script)
 (synopsis "A slow scripting language")
 (description "A longer description")
 (depends
   ; Dynarray module requires 5.2.0
   ; Checked 2026-02-20
   (ocaml (and (>= 5.4.0) (< 5.5.0)))
   dune
   ; Checked 2026-02-20
   (ounit2 (and (>= 2.2.7) (< 3.0.0)))
   ; Checked 2026-02-20
   (core (and (>= v0.17.1) (< v0.18)))
   ; Checked 2026-02-20
   (core_unix (and (>= v0.17.1) (< v0.18)))
   ; Checked 2026-02-20
   (ppx_jane (and (>= v0.17.0) (< v0.18)))
   ; Checked 2026-02-20
   (re (and (>= 1.14.0) (< 2.0.0)))
   ; Checked 2026-02-20
   (readline (>= 0.2))
 )
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
