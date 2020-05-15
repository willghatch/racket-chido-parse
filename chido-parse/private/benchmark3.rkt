#lang racket/base

(module+ main
  (require
   "core-use.rkt"
   "procedural-combinators.rkt"
   "readtable-parser.rkt"
   (submod "readtable-parser.rkt" racket-like-readtable)
   "parameters.rkt"
   racket/cmdline
   racket/port
   racket/file
   racket/stream
   rackunit
   )

  (define my-parser (chido-readtable->read* racket-like-readtable))

  (define f (command-line #:args (filename) filename))

  (define s (file->string f))
  (eprintf "input length: ~v characters\n" (string-length s))

  (collect-garbage 'major)
  (eprintf "Time for chido-parse s-exp parser to get stream:\n")
  (define my-parse-stream (time (whole-parse* (open-input-string s) my-parser)))
  (eprintf "Time for chido-parse s-exp parser to get stream-first:\n")
  (define sf (time (stream-first my-parse-stream)))
  (eprintf "Time for chido-parse s-exp parser to get semantic result:\n")
  (define my-parse-result (time (parse-derivation-result sf)))
  (get-counts!)
  (eprintf "Time for chido-parse s-exp parser to get verification of empty rest-of-stream:\n")
  (time (stream-empty? (stream-rest my-parse-stream)))
  (get-counts!)
  (collect-garbage 'major)
  (eprintf "Time for racket's read function:\n")
  (define r-parse
    (time (port->list (λ (p) (read-syntax "TODO-need-port-name-here" p))
                      (open-input-string s))))

  (check-equal? (map syntax->datum (syntax->list my-parse-result)) (map syntax->datum r-parse))

  )
