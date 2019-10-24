#lang racket/base

(provide
 se?
 se/datum?
 )

(require
 racket/match
 )

(define (se? a b #:loc? [loc? #t])
  ;; syntax-equal? for datum and position.
  ;; Ignores syntax properties.
  (define (rec a b)
    (se? a b #:loc? loc?))
  (define (ifloc? yes [no #t])
    (if loc? yes no))
  (or
   (and (match (list a b)
          [(list (list ai ...) (list bi ...))
           (andmap rec ai bi)]
          [(list (? syntax?) b) #f]
          [else (equal? a b)]))
   (and (syntax? a)
        (syntax? b)
        (rec (syntax-e a) (syntax-e b))
        (ifloc? (equal? (syntax-source a) (syntax-source b)))
        (ifloc? (equal? (syntax-line a) (syntax-line b)))
        (ifloc? (equal? (syntax-column a) (syntax-column b)))
        (ifloc? (equal? (syntax-position a) (syntax-position b)))
        (ifloc? (equal? (syntax-span a) (syntax-span b))))
   (begin
     #;(if (and (syntax? a) (syntax? b))
         (eprintf "different: ~s while expected ~s\n"
                  (list a (syntax->datum a) (syntax-source a)
                        (syntax-line a) (syntax-column a)
                        (syntax-position a) (syntax-span a))
                  (list b (syntax->datum b) (syntax-source b)
                        (syntax-line b) (syntax-column b)
                        (syntax-position b) (syntax-span b)))
         (eprintf "different: ~s while expected ~s\n" a b))
     #f)))

(define (se/datum? a b)
  (se? a b #:loc? #f))
