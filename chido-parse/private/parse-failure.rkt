#lang racket/base

(provide
 (struct-out parse-failure)
 greatest-failure
 parse-failure-greater-than?
 parse-failure->chain
 parse-failure->first-message
 )
(require
 racket/stream
 ;racket/list
 )

(struct parse-failure
  (parser
   ;; message can be #f or a string.  If it's #f, we look to its inner-failure for a message.
   message
   report-line
   report-column
   report-position
   effective-start effective-end
   made-progress?
   inner-failure
   ;; inner-failure-list is either a list of all failures that lead to this failure
   ;; (instead of just the best), or #f.
   inner-failure-list
   )
  ;#:transparent
  #:methods gen:stream
  [(define (stream-empty? s) #t)
   ;; TODO - better error messages
   (define (stream-first s)
     (error 'stream-first "empty stream"))
   (define (stream-rest s)
     (error 'stream-rest "empty stream"))])


(define (parse-failure-greater-than? greater lesser)
  ;; TODO - is this the best way to decide this?
  ;;        Should there also be other kinds of scoring?  Eg. attach extra info to failures when creating them to say how much progress they made?
  (define (greater-for-progress? g l)
    (and (parse-failure-made-progress? greater)
         (not (parse-failure-made-progress? lesser))))
  (define (greater-for-end? g l)
    (> (parse-failure-effective-end g)
       (parse-failure-effective-end l)))
  (define (greater-for-start? g l)
    (> (parse-failure-effective-start g)
       (parse-failure-effective-start l)))

  (or (greater-for-progress? greater lesser)
      (and (not (greater-for-progress? lesser greater))
           (or (greater-for-end? greater lesser)
               (and (not (greater-for-end? lesser greater))
                    #;(greater-for-start? greater lesser))))))

(define (greatest-failure failures
                          #:default [default (λ () (error 'greatest-failure
                                                          "no default given"))])
  (if (null? failures)
      (if (procedure? default)
          (default)
          default)
      (let* ([sorted (sort failures parse-failure-greater-than?)]
             #;[greatest-ties (takef sorted (λ (x) (not (parse-failure-greater-than?
                                                       (car sorted)
                                                       x))))])
        #;(when (not (null? (cdr greatest-ties)))
          (eprintf "failure sort ties: ~v\n" (map parse-failure->first-message
                                                  greatest-ties)))
        (car sorted))))

(define (parse-failure->chain pf)
  (if (not pf)
      '()
      (cons pf (parse-failure->chain (parse-failure-inner-failure pf)))))

(define (parse-failure->first-message pf)
  (cond [(not pf) #f]
        [(parse-failure-message pf)]
        [else (parse-failure->first-message (parse-failure-inner-failure pf))]))
