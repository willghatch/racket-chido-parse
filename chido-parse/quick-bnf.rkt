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
                 (let ([m (regexp-match #px"\\w+"
                                        port)])
                   (and m
                        (datum->syntax
                         #f
                         (->sym m)
                         (list (object-name port) line col pos
                               (string-length (symbol->string (->sym m))))))))))

(module+ test
  (check se/datum?
         (wp*/r "test_id"
                id-parser)
         (list #'test_id))
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

(define-bnf/quick cpqb-parser
  [top-level [arm +]]
  [arm [id-parser ":" alt-sequence]]
  [alt-sequence [alt @ #(/ "|" alt)
                       ;+
                       ;; * is correct, but + is making it do an infinite loop, and I want to know why.
                       *
                       ]]
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
                   ;["(" elem #("|" elem) + ")"]
                   ["(" elem + ")"]]
  )

(module+ test
  (define bnf-string-1 "
stmt: \"pass\"
")

  (eprintf "here1\n")
  ;; TODO - figuring out what's wrong probably requires another pass at working with failures... probably just figure out the rightmost failure at each juncture and only propagate that.
  ;; TODO - I am infinitely looping without ever generating a first result.  Perhaps some parser is being evaluated multiple times, giving me a fresh parser somewhere, but that parser is either never succeeding or is being filtered out somewhere higher, which keeps asking for more parses.  But where?
  (eprintf "got: ~v\n"
           (parse* (open-input-string bnf-string-1) cpqb-parser))
  (eprintf "here2\n")


  (define bnf-string/stmt "
stmt : \"pass\"
     | expression
     | \"{\" stmt + \"}\"
expr : bnumber
     | expr \"+\" expr & left
     | expr \"*\" expr & left > +
bnumber : (\"0\" | \"1\") +
")

  (eprintf "before-broken-test\n")
  (define result (whole-parse* (open-input-string bnf-string/stmt) cpqb-parser))
  (printf "~v\n" (parse-derivation-result (stream-ref result 0)))


  #;(check se/datum?
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
