#lang racket/base

(provide
 (all-defined-out)
 (all-from-out "test-util-2.rkt"))

(require
 "core.rkt"
 "procedural-combinators.rkt"
 "test-util-2.rkt"
 )

(define (wp*/r str parser)
  (->results (whole-parse* (open-input-string str) parser)))