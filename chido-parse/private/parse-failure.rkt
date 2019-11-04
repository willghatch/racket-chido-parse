#lang racket/base

(provide
 (struct-out parse-failure)
 greatest-failure
 parse-failure-greater-than?
 )
(require
 racket/stream
 )

(struct parse-failure
  (parser
   ;; message can be #f or a string.  If it's #f, we look to its inner-failure for a message.
   current-message
   report-position
   effective-start effective-end
   made-progress?
   inner-failure
   ;; inner-failure-list is either a list of all failures that lead to this failure
   ;; (instead of just the best), or #f.
   inner-failure-list
   )
  #:transparent
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
  (or (and (parse-failure-made-progress? greater)
           (not (parse-failure-made-progress? lesser)))
      (> (parse-failure-effective-end greater)
         (parse-failure-effective-end lesser))
      (> (parse-failure-effective-start greater)
         (parse-failure-effective-start lesser))))

(define (greatest-failure failures
                          #:default [default (λ () (error 'greatest-failure
                                                          "no default given"))])
  (if (null? failures)
      (if (procedure? default)
          (default)
          default)
      (car (sort failures parse-failure-greater-than?))))

#|
TODO - extra helpers
failure views
* parse-failure->string/simple -- gives the location, name of failed parser, and message if not #f
* parse-failure->string/message -- like simple, but searches the chain for the first message it finds.
* parse-failure->string/chain -- reports name of failed parser and message for each failure in the chain
|#
