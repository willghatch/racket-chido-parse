#lang racket/base

(provide
 (rename-out [cpqb-parser quick-bnf-parser]))


(require
 "core.rkt"
 "procedural-combinators.rkt"
 "bnf.rkt"
 "readtable-parser.rkt"
 ;; TODO - use the “default” chido-parse s-exp readtable, which I should make...
 (submod "readtable-parser.rkt" an-s-exp-readtable)
 )
(module+ test
  (require
   rackunit
   racket/stream
   "test-util-3.rkt"
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
              (parse* (open-input-string "\"stringtest\"") id-parser)))

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

(define default-s-exp-parser an-s-exp-readtable)
(define lisp-escape-parser
  (proc-parser
   #:prefix "$"
   (λ (port) (parse* port default-s-exp-parser))))

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
                     #(/"(" string-parser + /")"))]
            ["::" default-s-exp-parser]]
  [elem [#(id-parser "=") ?
         "/" ?
         "@" *
         compound-parser
         "?" ?
         "*" ?
         "+" ?]]
  [compound-parser [id-parser]
                   [string-parser]
                   [lisp-escape-parser]
                   [/ "(" elem @@ #(/ "|" elem) + / ")"
                      :: (λ elems (cons 'ELEM-ALT elems))]
                   [/ "(" @ elem + / ")"
                      :: (λ elems (cons 'ELEM-LIST elems))]]
  )

(module+ test
  (define bnf-string-1 "
stmt: \"pass\"
")
  (check se/datum?
         (wp*/r bnf-string-1 cpqb-parser)
         (list #'([stmt ":" [((() () () "pass" () () ())) ()]])))

  (check se/datum?
         (wp*/r "something: $(in lisp escape)" cpqb-parser)
         (list #'([something ":" [((() () () (in lisp escape) () () ())) ()]])))



  (define bnf-string/stmt "
stmt : \"pass\"
     | expr
     | \"{\" stmt + \"}\"
expr : $(follow-filter bnumber bnumber)
     | expr \"+\" expr & left
     | expr \"*\" expr & left > \"+\"
bnumber : (\"0\" | \"1\") +
          :: (λ (elems) (list (apply string-append (syntax->datum elems))))
")

  (define result (whole-parse* (open-input-string bnf-string/stmt) cpqb-parser))


  (check se/datum?
         (map parse-derivation-result
              (stream->list
               (whole-parse* (open-input-string bnf-string/stmt
                                                ;; use this name to make it easy to compare syntax output when they differ -- it's the same length
                                                "aaaaaaaaaaaaaaaaaaaaaA")
                             cpqb-parser)))
         (list #'([stmt ":"
                        {([() () () "pass" () () ()]) ()}
                        {([() () () expr () () ()]) ()}
                        {([() () () "{" () () ()]
                          [() () () stmt () () ("+")]
                          [() () () "}" () () ()])
                         ()}]
                  [expr ":"
                        {([() () ()  (follow-filter bnumber bnumber) () () ()]) ()}
                        {([() () () expr () () ()]
                          [() () () "+" () () ()]
                          [() () () expr () () ()])
                         (["&" "left"])}
                        {([() () () expr () () ()]
                          [() () () "*" () () ()]
                          [() () () expr () () ()])
                         (["&" "left"] [">" "+"])}
                        ]
                  [bnumber ":"
                           {([() () () (ELEM-ALT [() () () "0" () () ()]
                                                 [() () () "1" () () ()]
                                                 )
                                 () () ("+")])
                            (("::" (λ (elems)
                                     (list (apply string-append
                                                  (syntax->datum elems))))))}]
                  )))

  )

