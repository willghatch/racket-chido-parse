#lang racket/base

(require
 chido-parse
 (submod chido-parse/private/readtable-parser an-s-exp-readtable)
 )

(define dynamic-parse-loader
  (proc-parser
   (λ (port)
     (define prefix (parse*-direct port "◊"))
     ;; Read a module name as a symbol.
     (define symbol (parse*-direct port current-chido-readtable-symbol-parser))
     (define dynamic-parser
       ;; Dynamically load a parser from the module specified
       ;; in the file we are currently parsing.
       (dynamic-require (syntax->datum (parse-derivation-result symbol))
                        'dynamic-parser))
     (define open-brace (parse*-direct port "{"))
     (define dynamic-parse (parse*-direct port dynamic-parser))
     (define close-brace (parse*-direct port "}"))
     (make-parse-derivation
      (parse-derivation-result dynamic-parse)
      #:derivations (list prefix symbol
                          open-brace dynamic-parse close-brace)))))

(module+ test

  (require
   rackunit
   "../private/test-util-3.rkt"
   )

  (check se/datum?
         (chido-parse-parameterize
          ([current-chido-readtable an-s-exp-readtable])
          (wp*/r "◊chido-parse/demo/unary{1111111}" dynamic-parse-loader))
         (list #'7))

  )
