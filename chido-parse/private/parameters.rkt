#lang racket/base

(provide
 current-chido-parse-parameters
 chido-parse-parameter?
 chido-parse-parameter
 chido-parse-parameterize
 with-chido-parse-parameters
 )

(require
 (for-syntax
  racket/base
  syntax/parse
  ))

(define chido-parse-param-key 'chido-parse)

(define (current-chido-parse-parameters)
  (continuation-mark-set-first #f chido-parse-param-key #hasheq()))

(define-syntax-rule (with-chido-parse-parameters params body ...)
  (with-continuation-mark
   chido-parse-param-key params
   (let () body ...)))

(struct chido-parse-parameter (default)
  #:property prop:procedure
  (λ (p) (hash-ref (current-chido-parse-parameters)
                   p
                   (chido-parse-parameter-default p))))

(define-syntax (chido-parse-parameterize stx)
  (syntax-parse stx
    [(_ ([param val] ...) (~describe "body expression" body:expr) ...+)
     (with-syntax ([(pv ...) (generate-temporaries #'(param ...))])
       #'(let ([pv param] ...)
           (and (or (chido-parse-parameter? pv)
                    (error 'chido-parse-parameterize "not a chido-parse-parameter: ~a" pv))
                ...)
           (with-chido-parse-parameters
             (for/fold ([h (current-chido-parse-parameters)])
                       ([p (list pv ...)]
                        [v (list val ...)])
               (hash-set h p v))
             body ...)))]))


(module+ test
  (require rackunit)
  (define tp1 (chido-parse-parameter 'hello))
  (define tp2 (chido-parse-parameter 'bye))

  (check-equal? (tp1) 'hello)
  (check-equal? (chido-parse-parameterize ([tp1 'foo])
                                          (tp1))
                'foo)
  (check-equal? (chido-parse-parameterize ([tp1 'foo])
                                          (tp2))
                'bye)
  (check-equal? (chido-parse-parameterize ([tp1 'foo]
                                           [tp2 'bar])
                                          (list (tp1) (tp2)
                                                (chido-parse-parameterize ([tp1 'baz])
                                                                          (tp1))))
                '(foo bar baz))
  )
