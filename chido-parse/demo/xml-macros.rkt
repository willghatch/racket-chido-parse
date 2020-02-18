#lang racket/base

(require
 ;; for xexpr stuff
 xml

 syntax/parse/define
 (for-syntax
  racket/base
  syntax/parse
  ))

(provide
 (all-defined-out)
 )
(module+ with-racket-base
  (require racket/base)
  (provide (all-from-out racket/base)
           document
           element
           EntityRef
           CharRef
           ))


;; Macros to convert the output of my xml parser to x-exprs

(define-syntax-parser document
  [(_ prolog-form top-level-element)
   ;; TODO - actually use the prolog
   #'(let ([document-content top-level-element])
       ;(printf "got xexpr: ~v\n" document-content)
       ;(printf "as xml: ~v\n" (xexpr->string document-content))
       document-content)])
#;(define-synatx-parser prolog
  TODO)

(define-syntax-parser element
  [(_ tag:id ((attribute-name:id attribute-value:str) ...) contents ...)
   #'(list 'tag '((attribute-name attribute-value) ...)
           contents ...)])

(define-syntax-parser EntityRef
  [(_ (~datum lt)) #'"<"]
  [(_ (~datum gt)) #'">"]
  [(_ (~datum amp)) #'"&"]
  [(_ (~datum apos)) #'"'"]
  [(_ (~datum quot)) #'"\""]
  [stx (raise-syntax-error 'EntityRef "unsupported XML entity reference" #'stx)])
(define-syntax-parser CharRef
  [(_ n:number)
   #'(string (integer->char n))])




(module reader syntax/module-reader
  (submod chido-parse/demo/xml-macros with-racket-base)
  #:read-syntax xml-read-syntax
  #:read xml-read
  #:whole-body-readers? #t
  (require "xml.rkt" chido-parse)
  (define (xml-read-syntax src port)
    (define parse-result
      (whole-parse port (bnf-parser->with-surrounding-layout parser)))
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
