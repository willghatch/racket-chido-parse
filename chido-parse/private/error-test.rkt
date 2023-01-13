#lang racket/base

(module+ main
  (require
   "core-use.rkt"
   "procedural-combinators.rkt"
   "readtable-parser.rkt"
   (submod "readtable-parser.rkt" an-s-exp-readtable)
   "parameters.rkt"
   racket/cmdline
   racket/port
   racket/file
   racket/stream
   rackunit
   )

  (define my-parser (chido-readtable->read* an-s-exp-readtable))

  (define f (command-line #:args (filename) filename))

  (define s (file->string f))
  (define result (whole-parse* (open-input-string s) my-parser))
  (if (parse-failure? result)
      (printf "~a" (parse-failure->string/chain result))
      (printf "not a failure...\n"))


  )
