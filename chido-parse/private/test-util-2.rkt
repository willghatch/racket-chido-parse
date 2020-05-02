#lang racket/base

(provide
 (all-defined-out)
 (all-from-out "test-util-1.rkt"))

(require
 racket/stream
 "test-util-1.rkt"
 "core-use.rkt"
 )


(define (->results ds)
    (map parse-derivation-result (stream->list ds)))

(define (p*/r str parser)
  (->results (parse* (open-input-string str) parser)))
