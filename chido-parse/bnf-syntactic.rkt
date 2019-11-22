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
   "private/readtable-parser.rkt"
   ;; TODO - use default chido-parse readtable
   (submod "private/readtable-parser.rkt" an-s-exp-readtable)
   "private/bnf-parse.rkt"
   "private/bnf-macro.rkt"
   "private/bnf-s-exp.rkt"
   )
  (define (bnf-read-syntax src port)
    (strip-context
     (parse-derivation-result
      (whole-parse port
                   (sequence
                    (bnf-parser->with-surrounding-layout
                     syntactic-bnf-parser)
                    (repetition
                     #:min 0 #:max 1
                     (sequence
                      (result-filter an-s-exp-readtable
                                     (λ (stx) (keyword? (syntax-e stx))))
                      (chido-readtable->read* an-s-exp-readtable)
                      #:result/stx (λ (kw forms) (cons kw forms)))))))))
  (define (bnf-read port)
    (syntax->datum (bnf-read-syntax (object-name port) port)))
  )

(define-syntax (bnf-module-begin stx)
  (define-syntax-class not-keyword
    (pattern x #:when (not (keyword? (syntax-e #'x)))))
  (syntax-parse stx
    [(_ (bnf-form:not-keyword ...)
        ((~optional ((~and kw:keyword (~commit #:definitions))
                     lisp-form ...))))
     #'(#%module-begin
        (~? (begin lisp-form ...))
        (define-bnf/syntactic/parsed parser
          bnf-form ...)
        (provide parser))]))
