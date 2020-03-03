#lang racket/base

#|
I had a terrible bug with parse*-direct, which was demonstrated by the plus parser example I wanted to use.

This file has several variations of the bad parsers that I was using to try to figure out what was going wrong.
I figure I should commit it as a test.
|#

(require chido-parse)
(module+ test
  (require rackunit
           chido-parse/private/test-util-3
           racket/stream
           ))

(define e1-tracing? #f)
(define (e1printf . args)
  (when e1-tracing? (apply eprintf args)))

(module+ test
  (let ()
    ;; example 1 combinator alt
    (define (plus)
      (sequence expression "+" expression))
    (define (expression)
      (alt-parser plus number))
    (define (number)
      ;; This is a perfectly acceptable number parser.
      "4")


    (e1printf "results: ~v\n" (wp*/r "4+4+4" expression))
    (check-equal? (length (wp*/r "4+4+4" expression))
                  2)
    )
  (let ()
    (e1printf "\n\nexample 1 all for/parse\n")
    (define plus
      (proc-parser
       #:name "plus"
       (λ (port)
         (for/parse ([l (parse* port expression)])
                    (e1printf "~v: l is ~v\n"
                             (parse-derivation-start-position l)
                             (parse-derivation-result l))
                    (for/parse ([op (parse* port "+" #:start l)])
                               (e1printf "~v,~v: op is ~v\n"
                                        (parse-derivation-start-position l)
                                        (parse-derivation-start-position op)
                                        (parse-derivation-result op))
                               (for/parse ([r (parse* port expression #:start op)])
                                          (e1printf "~v,~v,~v: r is ~v\n"
                                                   (parse-derivation-start-position l)
                                                   (parse-derivation-start-position op)
                                                   (parse-derivation-start-position r)
                                                   (parse-derivation-result r))
                                          (make-parse-derivation
                                           `(+ ,(parse-derivation-result l)
                                               ,(parse-derivation-result r))
                                           #:derivations (list l op r))))))))
    (define (expression)
      (alt-parser plus number))
    (define (number)
      ;; This is a perfectly acceptable number parser.
      "4")


    (e1printf "results: ~v\n" (wp*/r "4+4+4" expression))
    (check-equal? (length (wp*/r "4+4+4" expression))
                    2)
    )
  (let ()
    (e1printf "\n\nl with parse*-direct\n")
    (define plus
      (proc-parser
       #:name "plus"
       (λ (port)
         (define l (parse*-direct port expression))
         (e1printf "~v: l (via parse*-direct) is: ~v\n"
                  (parse-derivation-start-position l)
                  (parse-derivation-result l))
         (for/parse ([op (parse* port "+" #:start l)])
                    (e1printf "~v,~v: op (via for/parse) is: ~v\n"
                             (parse-derivation-start-position l)
                             (parse-derivation-start-position op)
                             (parse-derivation-result op))
                    (for/parse ([r (parse* port expression #:start op)])
                               (e1printf "~v,~v,~v: r (via parse*-direct) is: ~v\n"
                                        (parse-derivation-start-position l)
                                        (parse-derivation-start-position op)
                                        (parse-derivation-start-position r)
                                        (parse-derivation-result r))
                               (make-parse-derivation
                                `(+ ,(parse-derivation-result l)
                                    ,(parse-derivation-result r))
                                #:derivations (list l op r)))))))
    (define (expression)
      (alt-parser plus number))
    (define (number)
      ;; This is a perfectly acceptable number parser.
      "4")


    (e1printf "results: ~v\n" (wp*/r "4+4+4" expression))
    #;(e1printf "results as derivations: ~v\n" (stream->list (whole-parse* "4+4+4" expression)))
    (check-equal? (length (wp*/r "4+4+4" expression))
                    2)
    )
  (let ()
    (e1printf "\n\nop with parse*-direct\n")
    (define plus
      (proc-parser
       #:name "plus"
       (λ (port)
         (for/parse ([l (parse* port expression)])
                    (e1printf "~v: l (via for/parse) is: ~v\n"
                             (parse-derivation-start-position l)
                             (parse-derivation-result l))
                    (define op (parse*-direct port "+" #:start l))
                    (e1printf "~v,~v: op (via for/parse) is: ~v\n"
                             (parse-derivation-start-position l)
                             (parse-derivation-start-position op)
                             (parse-derivation-result op))
                    (for/parse ([r (parse* port expression #:start op)])
                               (e1printf "~v,~v,~v: r (via parse*-direct) is: ~v\n"
                                        (parse-derivation-start-position l)
                                        (parse-derivation-start-position op)
                                        (parse-derivation-start-position r)
                                        (parse-derivation-result r))
                               (make-parse-derivation
                                `(+ ,(parse-derivation-result l)
                                    ,(parse-derivation-result r))
                                #:derivations (list l op r)))))))
    (define (expression)
      (alt-parser plus number))
    (define (number)
      ;; This is a perfectly acceptable number parser.
      "4")


    (e1printf "results: ~v\n" (wp*/r "4+4+4" expression))
    #;(e1printf "results as derivations: ~v\n" (stream->list (whole-parse* "4+4+4" expression)))
    (check-equal? (length (wp*/r "4+4+4" expression))
                    2)
    )
  (let ()
    (e1printf "\n\nr with parse*-direct\n")
    (define plus
      (proc-parser
       #:name "plus"
       (λ (port)
         (for/parse ([l (parse* port expression)])
                    (e1printf "~v: l (via for/parse) is: ~v\n"
                             (parse-derivation-start-position l)
                             (parse-derivation-result l))
                    (for/parse ([op (parse* port "+" #:start l)])
                               (e1printf "~v,~v: op (via for/parse) is: ~v\n"
                                        (parse-derivation-start-position l)
                                        (parse-derivation-start-position op)
                                        (parse-derivation-result op))
                               (define r (parse*-direct port expression #:start op))
                               (e1printf "~v,~v,~v: r (via parse*-direct) is: ~v\n"
                                        (parse-derivation-start-position l)
                                        (parse-derivation-start-position op)
                                        (parse-derivation-start-position r)
                                        (parse-derivation-result r))
                               (make-parse-derivation
                                `(+ ,(parse-derivation-result l)
                                    ,(parse-derivation-result r))
                                #:derivations (list l op r)))))))
    (define (expression)
      (alt-parser plus number))
    (define (number)
      ;; This is a perfectly acceptable number parser.
      "4")


    (e1printf "results: ~v\n" (wp*/r "4+4+4" expression))
    #;(e1printf "results as derivations: ~v\n" (stream->list (whole-parse* "4+4+4" expression)))
    (check-equal? (length (wp*/r "4+4+4" expression))
                    2)
    )
  (let ()
    (e1printf "\n\nexample 1 op and r with parse*-direct\n")
    (define plus
      (proc-parser
       #:name "plus"
       (λ (port)
         (for/parse ([l (parse* port expression)])
                    (define op (parse*-direct port "+" #:start l))
                    (define r (parse*-direct port expression #:start op))
                    (make-parse-derivation
                     `(+ ,(parse-derivation-result l)
                         ,(parse-derivation-result r))
                     #:derivations (list l op r))))))
    (define (expression)
      (alt-parser plus number))
    (define (number)
      ;; This is a perfectly acceptable number parser.
      "4")


    (e1printf "results: ~v\n" (wp*/r "4+4+4" expression))
    (check-equal? (length (wp*/r "4+4+4" expression))
                    2)
    )
  (let ()
    (e1printf "\n\nexample 1 l and r as parse*-direct\n")
    (define plus
      (proc-parser
       #:name "plus"
       (λ (port)
         (define l (parse*-direct port expression))
         (for/parse ([op (parse* port "+" #:start l)])
                    (define r (parse*-direct port expression #:start op))
                    (make-parse-derivation
                     `(+ ,(parse-derivation-result l)
                         ,(parse-derivation-result r))
                     #:derivations (list l op r))))))
    (define (expression)
      (alt-parser plus number))
    (define (number)
      ;; This is a perfectly acceptable number parser.
      "4")


    (e1printf "results: ~v\n" (wp*/r "4+4+4" expression))
    (check-equal? (length (wp*/r "4+4+4" expression))
                    2)
    )
  (let ()
    (e1printf "\n\nexample 1 l and op as parse*-direct\n")
    (define plus
      (proc-parser
       #:name "plus"
       (λ (port)
         (define l (parse*-direct port expression))
         (define op (parse*-direct port "+" #:start l))
         (for/parse ([r (parse* port expression #:start op)])
                    (make-parse-derivation
                     `(+ ,(parse-derivation-result l)
                         ,(parse-derivation-result r))
                     #:derivations (list l op r))))))
    (define (expression)
      (alt-parser plus number))
    (define (number)
      ;; This is a perfectly acceptable number parser.
      "4")


    (e1printf "results: ~v\n" (wp*/r "4+4+4" expression))
    (check-equal? (length (wp*/r "4+4+4" expression))
                    2)
    )
  (let ()
    (e1printf "\n\nexample 1 all parse*-direct but with explicit positions\n")
    (define plus
      (proc-parser
       #:name "plus"
       (λ (port)
         (define l (parse*-direct port expression))
         (define op (parse*-direct port "+" #:start l))
         (define r (parse*-direct port expression #:start op))
         (make-parse-derivation
          `(+ ,(parse-derivation-result l)
              ,(parse-derivation-result r))
          #:derivations (list l op r)))))
    (define (expression)
      (alt-parser plus number))
    (define (number)
      ;; This is a perfectly acceptable number parser.
      "4")


    (e1printf "results: ~v\n" (wp*/r "4+4+4" expression))
    (check-equal? (length (wp*/r "4+4+4" expression))
                    2)
    )
  )


;; examples.scrbl example 1

(define plus
  (proc-parser
   #:name "plus"
   (λ (port)
     (define l (parse*-direct port expression))
     (define op (parse*-direct port "+"))
     (define r (parse*-direct port expression))
     (make-parse-derivation
      `(+ ,(parse-derivation-result l)
          ,(parse-derivation-result r))
      #:derivations (list l op r)))))
(define (expression)
  (alt-parser plus number))
#;(define (number)
  ...)

;; replace stub with something that works
(define (number)
  ;; This is a perfectly acceptable number parser.
  "4")

(module+ test
  (check-equal? (wp*/r "4+4" expression)
                `[(+ "4" "4")])
  (check-equal? (p*/r "4+4" expression)
                `["4" (+ "4" "4")])

  (chido-parse-keep-multiple-failures? #t)

  (check-equal? (length (wp*/r "4+4+4" expression))
                2)

  )


;; require for example 2
(require racket/list)
;; examples.scrbl example 2
(define filtered-plus
  (derivation-filter
   plus
   (λ (d)
     ;; Filter to make plus right-associative.
     (not (equal? (parse-derivation-parser
                     (first
                      (parse-derivation-subderivations d)))
                    plus)))))

(module+ test
  (check-equal? (wp*/r "4+4+4" filtered-plus)
                `[(+ "4" (+ "4" "4"))])
  )
