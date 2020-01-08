#lang racket/base
;; This is mostly as e helper to test dynamic-parser.

(provide (rename-out [unary-parser dynamic-parser]))

(require chido-parse)

(define unary-parser
  (repetition "1"
              #:name "unary-parser" #:greedy? #t
              #:result/stx (λ (ones) (length ones))))

(module+ test
  (require rackunit "../private/test-util-3.rkt")
  (check se/datum?
         (wp*/r "11111" unary-parser)
         (list #'5))
  )
