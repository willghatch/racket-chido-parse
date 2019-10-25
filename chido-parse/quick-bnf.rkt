#lang racket/base
(require
 "private/core.rkt"
 "private/procedural-combinators.rkt"
 "private/bnf.rkt"
 )
(module+ test
  (require
   rackunit
   racket/stream
   ))

#|
* Make a reader module
* Make macros to desugar to define-bnf/quick

|#

(define id-parser
  (proc-parser #:name "id-parser"
               (λ (port)
                 (let ([m (regexp-match #px"\\w+"
                                        port)])
                   (and m (bytes->string/utf-8 (car m)))))))

(module+ test
  (check-equal? (map parse-derivation-result
                     (stream->list
                      (parse* (open-input-string "test_id")
                              id-parser)))
                '("test_id")))

(define-bnf cpqb-parser
  [top-level [[: #:repeat-min 1 arm]]]
  [arm [id-parser ":" alt-sequence]]
  [alt-sequence [alt [: #:splice 1 #:repeat-min 0
                        (binding-sequence [: #:ignore #t "|"] alt)]]]
  [alt [[: #:repeat-min 1 elem] [: #:repeat-min 0 alt-flag]]]
  [alt-flag (TODO -- IE assoc/precidence)]
  [elem [[: #:splice 1 #:repeat-max 1 (binding-sequence id-parser "=")]
         [: #:repeat-max 1 "/"]
         [: #:repeat-min 0 "@"]
         base-parser ;; TODO - this should have alt capabilities -- eg. (a | b)
         [: #:repeat-max 1 "?"]
         [: #:repeat-max 1 "+"]
         [: #:repeat-max 1 "*"]]]
  )
