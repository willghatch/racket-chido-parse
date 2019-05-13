#lang racket/base

(provide
 parse-stream-cons
 for/parse
 )

(require
 racket/stream
 "parameters.rkt"
 (for-syntax
  racket/base
  syntax/parse
  ))


(define-syntax (parse-stream-cons stx)
  (syntax-parse stx
    [(_ head:expr tail:expr)
     #'(let ([cp-params (current-chido-parse-parameters)])
         (stream-cons (parameterize ([current-chido-parse-parameters cp-params])
                        head)
                      (parameterize ([current-chido-parse-parameters cp-params])
                        tail)))]))

(define (for/parse-proc body-proc arg-stream-thunk)
  (let loop ([stream (arg-stream-thunk)])
    (if (stream-empty? stream)
        stream
        (let ([v1 (stream-first stream)])
          (parse-stream-cons (body-proc v1)
                             (loop (stream-rest stream)))))))
(define-syntax (for/parse stx)
  (syntax-parse stx
    [(_ ([arg-name input-stream])
        body ...+)
     #'(for/parse-proc (λ (arg-name) body ...)
                       (λ () input-stream))]))
