(lang dune 3.10)

(name helloworld)

(generate_opam_files true)

(source
 (github edwintorok/gitpod-ocaml-test))

(license LICENSE)

(package
 (name helloworld)
 (depends ocaml dune domainslib))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
