#lang racket/base

(provide
 (struct-out parse-failure)
 )
(require
 racket/stream
 )

(struct parse-failure
  ;; TODO -- what should this contain?
  (name start-position fail-position message sub-failures)
  #:transparent
  #:methods gen:stream
  [(define (stream-empty? s) #t)
   ;; TODO - better error messages
   (define (stream-first s)
     (error 'stream-first "empty stream"))
   (define (stream-rest s)
     (error 'stream-rest "empty stream"))])
