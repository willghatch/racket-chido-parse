#lang racket/base
(require
 (rename-in "test-bnf-syntactic-lang.rkt"
            [parser stmt-test])
 "core.rkt"
 "bnf-s-exp.rkt"
 )


(module+ test
  (require
   rackunit
   "test-util-3.rkt"
   )

  (check se/datum?
         (wp*/r "{ 10 + 11 pass 11 * 11 + 110}" stmt-test)
         (list #'("{" ("10" "+" "11")
                      "pass"
                      (("11" "*" "11") "+" "110")  "}")))
  )
