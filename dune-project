(lang dune 3.1)

(name MoreOCaml)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name MoreOCaml)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune ounit2)
 (allow_empty true)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
