#lang racket/base

(require
 "scheduler.rkt"
 "forparse.rkt"
 racket/string
 )

(module+ test
  (require
   rackunit
   ))

(define (sequence-2 l r
                    #:name [name #f]
                    #:derive [derive #f]
                    #:result [result #f])
  (define prefix (if (proc-parser? l)
                     (proc-parser-prefix l)
                     ""))
  (define use-name (or name (format "sequence-2_~a_~a"
                                    (parser-name l)
                                    (parser-name r))))
  (define combiner
    (cond [(and derive result)
           (error 'sequence-2
                  "must provide either result or derive function, not both")]
          [derive derive]
          [result (λ (l-d r-d)
                    (make-parse-derivation
                     (result (parse-derivation-result l-d)
                             (parse-derivation-result r-d))
                     #:derivations (list l-d r-d)))]
          [else (error 'sequence-2
                       "must provide a result or derivation transformer")]))
  (define (proc port)
    (for/parse ([l-result (parse* port l)])
               (for/parse ([r-result (parse* port r #:start l-result)])
                          (combiner l-result r-result))))
  (proc-parser use-name prefix proc))

(define (sequence #:name [name #f]
                  #:derive [derive #f]
                  #:result [make-result #f]
                  . parsers)
  (define prefix (if (proc-parser? (car parsers))
                     (proc-parser-prefix (car parsers))
                     ""))
  (define use-name (or name (format "sequence_~a"
                                    (string-join (map parser-name parsers) "_"))))
  (define combiner
    (cond [(and derive make-result)
           (error 'sequence
                  "must provide either result or derive function, not both")]
          [derive derive]
          [make-result (λ derivations
                         (make-parse-derivation
                          (apply make-result
                                 (map parse-derivation-result derivations))
                          #:derivations derivations))]
          [else (error 'sequence
                       "must provide a result or derivation transformer")]))
  (define (proc port)
    (define (rec parsers derivations)
      (cond [(null? parsers) (apply combiner (reverse derivations))]
            [else (for/parse ([result (parse* port (car parsers)
                                              #:start (if (null? derivations)
                                                          #f
                                                          (car derivations)))])
                             (rec (cdr parsers) (cons result derivations)))]))
    (rec parsers '()))
  (proc-parser use-name prefix proc))

(module+ test
  (define a (string->parser "a"))
  (define b "b")
  (define c "c")
  (define (B) (alt-parser "B"
                          (list b
                                (sequence #:name "Bb" #:result string-append
                                          B b))
                          (list '() '())))
  (define aBc (sequence #:name "aBc" #:result string-append
                        a B c))


  (define str1 "abbbbbbc")
  (define p1 (open-input-string str1))
  (for ([result (parse* p1 aBc)])
    (printf "result: ~a\n" result))
  )
