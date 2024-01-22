;;;  -*- lexical-binding: t -*-

(defconst-with-prefix fakemake
  feature 'base32
  authors "Dima Akater" author-email "nuclearspace@gmail.com"
  first-publication-year-as-string "2023"
  package-requires '((emacs "27.1") (cl-bytes "0.0.20240122.0"))
  org-files-in-order '("base32")
  site-lisp-config-prefix "50"
  license 'custom)
