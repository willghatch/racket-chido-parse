#lang racket/base
(require
 "private/core.rkt"
 "private/procedural-combinators.rkt"
 "private/readtable-parser.rkt"
 "private/bnf.rkt"
 )
(module+ test
  (require
   rackunit
   racket/stream
   "private/test-util-3.rkt"
   ))

#|
* Make a reader module
* Make macros to desugar to define-bnf/quick

|#

(define id-parser
  (proc-parser #:name "id-parser"
               (λ (port)
                 (define-values (line col pos) (port-next-location port))
                 (define (->sym m)
                   (string->symbol (bytes->string/utf-8 (car m))))
                 (let ([m (regexp-match #px"^\\w+"
                                        port)])
                   (if m
                       (datum->syntax
                        #f
                        (->sym m)
                        (list (object-name port) line col pos
                              (string-length (symbol->string (->sym m)))))
                       (make-parse-failure))))))

(module+ test
  (check se/datum?
         (wp*/r "test_id"
                id-parser)
         (list #'test_id))
  ;; I originally forgot to put the ^ on the regexp...
  (check-pred parse-failure?
              (parse* (open-input-string "\"stringtest\"") id-parser))
  )

(define string-parser
  (proc-parser #:name "string-parser"
               #:prefix "\""
               #:preserve-prefix? #t
               #:promise-no-left-recursion? #t
               (λ (port)
                 (read-syntax (object-name port) port))))

(module+ test
  (check se/datum?
         (wp*/r "\"test string\""
                string-parser)
         (list #'"test string")))

#|
TODO - I want a way to add symbols to the RESULT without having them in the PARSE.
Eg. [/"(" % ELEM-LIST @ elem + /")"]
-- after % it reads an item to be included verbatim in the list.
|#

(define-bnf/quick cpqb-parser
  [top-level-with-layout [/ current-chido-readtable-layout*-parser @ top-level / current-chido-readtable-layout*-parser]]
  [top-level [arm +]]
  [arm [id-parser ":" @ alt-sequence]]
  [alt-sequence [alt @@ #(/ "|" alt) *]]
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
                   ["(" @ elem @@@ #(/ "|" elem) + ")"]
                   [/ "(" @ elem + / ")"]]
  )

(module+ test
  (define bnf-string-1 "
stmt: \"pass\"
")
  (check se/datum?
         (wp*/r bnf-string-1 cpqb-parser)
         (list #'([stmt ":" [(("pass")) ()]])))



  (define bnf-string/stmt "
stmt : \"pass\"
     | expression
     | \"{\" stmt + \"}\"
expr : bnumber
     | expr \"+\" expr & left
     | expr \"*\" expr & left > \"+\"
bnumber : (\"0\" | \"1\") +
")

  (define result (whole-parse* (open-input-string bnf-string/stmt) cpqb-parser))


  (check se/datum?
         (map parse-derivation-result
              (stream->list
               (whole-parse* (open-input-string bnf-string/stmt   "aaaaaaaaaaaaaaaA")
                             cpqb-parser)))
         (list #'([stmt ":"
                        {(["pass"]) ()}
                        {([expression]) ()}
                        {(["{"] [stmt "+"] ["}"]) ()}]
                  [expr ":"
                        {([bnumber]) ()}
                        {([expr] ["+"] [expr]) (["&" "left"])}
                        {([expr] ["*"] [expr]) (["&" "left"] [">" "+"])}
                        ]
                  [bnumber ":"
                           {([("(" "0" "1" ")") "+"]) ()}]
                  )))

  )