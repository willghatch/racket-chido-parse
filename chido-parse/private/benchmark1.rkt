#lang racket/base

(module+ main
  (require
   "scheduler.rkt"
   "readtable-parser.rkt"
   "parameters.rkt"
   racket/cmdline
   racket/port
   racket/file
   racket/stream
   rackunit

   profile
   )

  (define my-s-exp-readtable
    (extend-chido-readtable*
     (chido-readtable-add-list-parser
      (chido-readtable-add-list-parser
       (chido-readtable-add-list-parser empty-chido-readtable "(" ")")
       "[" "]")
      "{" "}")
     'nonterminating hash-t-parser
     'nonterminating hash-f-parser
     'terminating racket-style-string-parser
     'layout " "
     'layout "\n"
     'layout "\t"))
  (define my-parser (chido-readtable->read1 my-s-exp-readtable))

  (define f (command-line #:args (filename) filename))

  (define s1 (file->string f))
  (define s "(
 (this is a test (of an s-expr that) is moderately
       (large and (I dont know) (whether (it will (be a (good) test)))
              but Im going to use it (anyway as) a test I guess))
 (this is a test (of an s-expr that) is moderately
       (large and (I dont know) (whether (it will (be a (good) test)))
              but Im going to use it (anyway as) a test I guess))
 (this is a test (of an s-expr that) is moderately
       (large and (I dont know) (whether (it will (be a (good) test)))
              but Im going to use it (anyway as) a test I guess))
 (this is a test (of an s-expr that) is moderately
       (large and (I dont know) (whether (it will (be a (good) test)))
              but Im going to use it (anyway as) a test I guess))
 (this is a test (of an s-expr that) is moderately
       (large and (I dont know) (whether (it will (be a (good) test)))
              but Im going to use it (anyway as) a test I guess))
 )")

  (eprintf "Time for chido-parse s-exp parser:\n")
  (define my-parse
    (chido-parse-parameterize
     ([current-chido-readtable my-s-exp-readtable])
     (time (parse-derivation-result
            (stream-first
             (parse* (open-input-string s) my-parser))))
     (eprintf "\n\nTime for SECOND parse of s-exp parse that should be cached!\n\n")
     (time (parse-derivation-result
            (stream-first
             (parse* (open-input-string s) my-parser))))))
  ;(eprintf "Time for racket's read function:\n")
  ;(define r-parse
  ;  (time (read (open-input-string s))))

  ;(check-equal? my-parse r-parse)

  )
