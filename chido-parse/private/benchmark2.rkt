#lang racket/base

(module+ main
  (require
   "core-use.rkt"
   "readtable-parser.rkt"
   "parameters.rkt"
   (prefix-in pb- "port-broker.rkt")
   racket/cmdline
   racket/port
   racket/file
   racket/stream
   rackunit

   profile
   )

  (define compare?
    (and
     ;; profile and don't compare if this is not commented out
     #f
     ))


  (require (submod "readtable-parser.rkt" racket-like-readtable))
  (define my-parser (chido-readtable->read1 racket-like-readtable))

  (define f (command-line #:args (filename) filename))

  (define s (file->string f))

  (eprintf "Time for chido-parse s-exp parser:\n")
  (define my-parse
    (let ()
      (define port (open-input-string s))
      (if compare?
          (time (parse-derivation-result
                 (stream-first
                  (parse* port my-parser))))
          (profile (parse-derivation-result
                    (stream-first
                     (parse* (open-input-string s) my-parser)))))))
  (when compare?
    (begin
      (collect-garbage 'major)
      (eprintf "Time for racket's read function:\n")
      (define p/orig (open-input-string s))
      (define pb (pb-port-broker p/orig))
      (define r-parse
        (time (read-syntax "name" (open-input-string s))))
      (collect-garbage 'major)
      (eprintf "time for Racket's read function with port broker:\n")
      (define r-parse-2
        (time (let ([p (pb-port-broker->port pb 1)])
                (read-syntax "name" p))))
      (collect-garbage 'major)
      (eprintf "time for Racket's read function with port broker again:\n")
      (define r-parse-3
        (time (let ([p (pb-port-broker->port pb 1)])
                (read-syntax "name" p))))
      (collect-garbage 'major)
      (eprintf "time for chido-parse with pre-used port broker:\n")
      (define cp-parse-2
        (time (let ([p (pb-port-broker->port pb 1)])
                (parse* p my-parser))))

      (check-equal? (syntax->datum my-parse) (syntax->datum r-parse))
      ))

  )
