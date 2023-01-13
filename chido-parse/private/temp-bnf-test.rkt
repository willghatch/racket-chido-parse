#lang chido-parse/bnf-syntactic

form : "a"
     | form * "b"

#:definitions
;; TODO - if I leave off the #:definitions, I get a `parse-derivation-result: not a parse-derivation: #<parse-failure>` error
(module* test racket/base
  (require
   (submod "..")
   chido-parse
   rackunit
   )

  (eprintf "~a\n" (parse-derivation-result (whole-parse (open-input-string "aab")
                                                        parser))))
