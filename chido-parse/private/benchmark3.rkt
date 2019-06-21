#lang racket/base

(module+ main
  (require
   "scheduler.rkt"
   "readtable-parser.rkt"
   (submod "readtable-parser.rkt" an-s-exp-readtable)
   "parameters.rkt"
   racket/cmdline
   racket/port
   racket/file
   racket/stream
   rackunit
   )

  (define my-parser (chido-readtable->read1 an-s-exp-readtable))

  (define f (command-line #:args (filename) filename))

  (define s (file->string f))

  (eprintf "Time for chido-parse s-exp parser:\n")
  (define my-parse
    (time (parse-derivation-result
           (stream-first
            (parse* (open-input-string s) my-parser)))))
  (get-counts!)
  (collect-garbage 'major)
  (eprintf "Time for racket's read function:\n")
  (define r-parse
    (time (read-syntax "TODO-need-port-name-here" (open-input-string s))))

  (check-equal? (syntax->datum my-parse) (syntax->datum r-parse))

  )
