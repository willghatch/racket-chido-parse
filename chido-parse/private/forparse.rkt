#lang racket/base

(provide for/parse)

(require
 racket/stream
 (for-syntax
  racket/base
  syntax/parse
  ))



(define (for/parse-proc body-proc arg-stream-thunk)
  (let loop ([stream (arg-stream-thunk)])
    (if (stream-empty? stream)
        stream
        (let ([v1 (stream-first stream)])
          (stream-cons (body-proc v1)
                       (loop (stream-rest stream)))))))
(define-syntax (for/parse stx)
  (syntax-parse stx
    [(_ ([arg-name input-stream])
        body ...+)
     #'(for/parse-proc (λ (arg-name) body ...)
                       (λ () input-stream))]))
