#lang racket/base

(provide
 parse*-direct-prompt
 delimit-parse*-direct
 parse-stream-cons
 for/parse-proc
 )

(require
 racket/stream
 "parameters.rkt"
 "parse-failure.rkt"
 (for-syntax
  racket/base
  syntax/parse
  ))

#|
The parse*-direct function needs its own continuation prompt.  When called during evaluation of a stream, it should NOT abort the processing of that stream to return its own stream instead.  The parse*-direct stream should be a child of any outer streams that are being processed.
|#
(define parse*-direct-prompt (make-continuation-prompt-tag 'parse*-direct-prompt))

(define-syntax (delimit-parse*-direct stx)
  (syntax-parse stx
    [(_ e:expr)
     #'(call-with-continuation-prompt
        (λ () e)
        parse*-direct-prompt)]))

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
                   (delimit-parse*-direct head))])
         ;; Note:  I previously used stream-cons here, which would be, uh,
         ;; more correct.  But stream cons onto a *custom* empty stream seems
         ;; to replace the custom empty stream with the canonical empty stream.
         ;; I get failure info out of some custom empty streams, so this is
         ;; unacceptable.  But I don't like using `stream` here either, because
         ;; now I'm making a stream tree instead of a flat stream.
         ;; It ultimately works out for me, because I flatten stream trees
         ;; (into the very custom stream that I get failure info from).
         ;; But if this could change, that would be great.
         (stream h
                 (parameterize ([current-chido-parse-parameters cp-params])
                   tail)))]))

(define (for/parse-proc body-proc arg-stream-thunk failure-proc)
  (let loop ([stream (arg-stream-thunk)])
    (cond [(parse-failure? stream) (failure-proc stream)]
          [(stream-empty? stream) stream]
          [else (let ([v1 (stream-first stream)])
                  (parse-stream-cons (body-proc v1)
                                     (loop (stream-rest stream))))])))
