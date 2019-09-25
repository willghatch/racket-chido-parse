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


#|
Parameters and streams don't work nicely together.
Not only do I need a special set of chido-parse-parameters to be captured by the
parsing system to influence caching and re-parameterize for recursive parses,
but I need to capture them and re-parameterize them with result streams too, or
later elements in the streams will get different parameterizations.
|#
(define-syntax (parse-stream-cons stx)
  ;; TODO - I want stream-cons to track failures!
  (syntax-parse stx
    [(_ head:expr tail:expr)
     #'(let* ([cp-params (current-chido-parse-parameters)]
              [h (parameterize ([current-chido-parse-parameters cp-params])
                   head)])
         (stream-cons h
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
