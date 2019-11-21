#lang racket/base
(provide
 (rename-out [bnf-module-begin #%module-begin])
 (except-out (all-from-out racket/base) #%module-begin)
 (all-from-out "main.rkt")
 )
(require
 "main.rkt"
 "private/bnf-macro.rkt"
 (for-syntax
  racket/base
  syntax/parse
  ))

(module reader syntax/module-reader
  chido-parse/bnf-syntactic
  #:read-syntax bnf-read-syntax
  #:read bnf-read
  #:whole-body-readers? #t
  (require
   syntax/strip-context
   racket/port
   "private/core.rkt"
   "private/procedural-combinators.rkt"
   "private/bnf-parse.rkt"
   "private/bnf-macro.rkt"
   "private/bnf-s-exp.rkt"
   )
  (define (bnf-read-syntax src port)
    (define parse-result
      (strip-context
       (parse-derivation-result
        (whole-parse port
                     (bnf-parser->with-surrounding-layout
                      syntactic-bnf-parser)))))
    (port->string port)
    parse-result)
  (define (bnf-read port)
    (syntax->datum (bnf-read-syntax (object-name port) port)))
  )

(define-syntax (bnf-module-begin stx)
  (syntax-parse stx
    [(_ form ...)
     #'(#%module-begin
        (define-bnf/syntactic/parsed parser
          form ...)
        (provide parser))]))
