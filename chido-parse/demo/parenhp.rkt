#lang racket/base

(require
 "xml-macros.rkt" 
 )

(provide
 (all-from-out  "xml-macros.rkt")
 (all-from-out racket/base)
 )

(module reader syntax/module-reader
  chido-parse/demo/parenhp
  #:read-syntax xml-read-syntax
  #:read xml-read
  #:whole-body-readers? #t
  (require
   "xml.rkt"
   chido-parse
   (submod chido-parse/private/readtable-parser an-s-exp-readtable)
   )

  (define basic-readtable an-s-exp-readtable)
  (define parenhp-s-exp
    (extend-chido-readtable 'terminating
                            (proc-parser
                             #:prefix "#<"
                             #:preserve-prefix? #t
                             (λ (port)
                               ;; consume #
                               (read-char port)
                               (parse* port (bnf-parser->arm-parser
                                             parenhp-parser
                                             'element))))
                            basic-readtable))

  (define parenhp-parser
    (extend-bnf
     parser
     [element ["<¿" parenhp-s-exp "¿>" #:result/stx (λ (l m r) m)]]))

  (define (xml-read-syntax src port)
    (define parse-result
      (whole-parse port (bnf-parser->with-surrounding-layout parenhp-parser)))
    (when (parse-failure? parse-result)
      (eprintf "~a\n" (parse-failure->string/chain parse-result))
      (error 'xml-read-syntax
             "Error parsing input file ~a at ~a."
             src
             (parse-failure->string/location-triple parse-result)))
    (list (parse-derivation-result parse-result)))
  (define (xml-read port)
    (syntax->datum (xml-read-syntax (object-name port) port)))
  )
