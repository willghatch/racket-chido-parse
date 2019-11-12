#lang racket/base
(require
 "private/core.rkt"
 "private/procedural-combinators.rkt"
 "private/bnf.rkt"
 )
(module+ test
  (require
   rackunit
   racket/stream
   "private/test-util-2.rkt"
   ))

#|
* Make a reader module
* Make macros to desugar to define-bnf/quick

|#

(define id-parser
  (proc-parser #:name "id-parser"
               (λ (port)
                 (let ([m (regexp-match #px"\\w+"
                                        port)])
                   (and m (bytes->string/utf-8 (car m)))))))

(module+ test
  (check-equal? (map parse-derivation-result
                     (stream->list
                      (parse* (open-input-string "test_id")
                              id-parser)))
                '("test_id")))

(define string-parser
  (proc-parser #:name "string-parser"
               #:prefix "\""
               #:preserve-prefix? #t
               #:promise-no-left-recursion? #t
               (λ (port)
                 (read-syntax (object-name port) port))))

(define-bnf/quick cpqb-parser
  [top-level [arm +]]
  [arm [id-parser ":" alt-sequence]]
  [alt-sequence [alt @ #(/ "|" alt) +]]
  [alt [elem + alt-flag *]]
  [alt-flag ["&" (|| "left" "right")]
            ;; TODO - associativity groups
            ["<" (|| string-parser
                     #(/"(" string-parser + /")"))]
            [">" (|| string-parser
                     #(/"(" string-parser + /")"))]]
  [elem [@ #(id-parser "=") ?
         @ "/" ?
         @ "@" *
         compound-parser
         @ "?" ?
         @ "*" ?
         @ "+" ?]]
  [compound-parser [id-parser]
                   [string-parser]
                   ;[lisp-escape-parser]
                   ["(" elem #("|" elem) + ")"]
                   ["(" elem + ")"]]
  )

(module+ test
  (define bnf-string-1 "
stmt: \"pass\"
")

  ;; TODO - figuring out what's wrong probably requires another pass at working with failures... probably just figure out the rightmost failure at each juncture and only propagate that.
  #;(eprintf "got: ~v\n"
           (parse* (open-input-string bnf-string-1) cpqb-parser))


  (define bnf-string/stmt "
stmt : \"pass\"
     | expression
     | \"{\" stmt + \"}\"
expr : bnumber
     | expr \"+\" expr & left
     | expr \"*\" expr & left > +
bnumber : (\"0\" | \"1\") +
")

  #;(eprintf "result: ~v\n"
           (whole-parse* (open-input-string bnf-string/stmt) cpqb-parser))

  (check se/datum?
         (map parse-derivation-result
              (stream->list
               (whole-parse* (open-input-string bnf-string/stmt)
                             cpqb-parser)))
         (list #'([stmt ":"
                        {(["pass"]) ()}
                        {([expression]) ()}
                        {(["{"] [stmt "+"] ["}"]) ()}
                        ]
                  [expr ":"
                        {([bnumber]) ()}
                        {([expr] ["+"] [expr]) (["&" "left"])}
                        {([expr] ["*"] [expr]) (["&" "left"] [">" "+"])}
                        ]
                  [bnumber ":"
                           {([{"(" "0" "|" "1" ")"}])}]
                  )))
  )
