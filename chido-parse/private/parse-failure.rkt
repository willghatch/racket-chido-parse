#lang racket/base

(provide
 (struct-out parse-failure)
 )
(require
 racket/stream
 )

(struct parse-failure
  ;; TODO -- what should this contain?
  #|
  TODO
  • I think I need failure objects that wrap a single other failure, eg. a sequence combinator has its third element fail.  It should wrap that failure object, and present a new failure with the same message and end position, but using the sequence start position.
  • Failures need more position info -- they need an effective start and end of the failure, but they also need a position to report to the user.  Eg. an unmatched closing paren needs to report the location of the paren, but its end position needs to be after the paren.
  • The failure sorting functions should be in this module.
  • I usually want to only propagate the best failures, but I want a mode to propagate ALL failures.  But even then it should be clear what the best failure is.
  |#
  (name start-position fail-position message sub-failures)
  #:transparent
  #:methods gen:stream
  [(define (stream-empty? s) #t)
   ;; TODO - better error messages
   (define (stream-first s)
     (error 'stream-first "empty stream"))
   (define (stream-rest s)
     (error 'stream-rest "empty stream"))])
