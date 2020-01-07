#lang racket/base

(provide
 syntactic-bnf-parser
 )


(require
 "core.rkt"
 "procedural-combinators.rkt"
 "bnf-s-exp.rkt"
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
  (check-pred parse-failure? (parse* (open-input-string "=") id-parser))
  (check se/datum?
         (map parse-derivation-result
              (stream->list (parse* (open-input-string "foo=") id-parser)))
         (list #'foo))
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

(define line-comment-parser
  (proc-parser
   #:prefix ";"
   (λ (port)
     (let loop ()
       (define c (read-char port))
       (if (or (eq? c #\newline)
               (eof-object? c))
           (make-parse-derivation #t)
           (loop))))))


(module+ test
  (check-equal? (wp*/r ";;; this is a line-comment-test"
                       line-comment-parser)
                '(#t)))

(define-bnf/quick syntactic-bnf-parser
  #:layout-parsers (list " " "\t" "\r" "\n" line-comment-parser)
  [top-level [@ arm +]]
  ;; TODO - I would like to use "/"? here, but it makes the parse ambiguous due to layout parsing differences.  What is the best way to fix that?
  [arm
   ["/" id-parser ":" @ alt-sequence]
   ["%" id-parser ":" @ alt-sequence]
   ["/" "%" id-parser ":" @ alt-sequence]
   ["%" "/" id-parser ":" @ alt-sequence]
   [id-parser ":" @ alt-sequence]]
  [/ alt-sequence [alt @@ #(/ "|" alt) *]]
  [alt [elem + alt-flag *]]
  [/ alt-flag
     ["&" (|| "left" "right")]
     ;; TODO - associativity groups
     ["<" (|| string-parser
              #(/"(" string-parser + /")"))]
     [">" (|| string-parser
              #(/"(" string-parser + /")"))]
     ["::" default-s-exp-parser]]
  [elem [#(id-parser "=") ?
         "/" ?
         "@" *
         @ compound-parser
         "?" ?
         "*" ?
         "+" ?]]
  [/ compound-parser
     [id-parser]
     [string-parser]
     [lisp-escape-parser]
     [/ "(" elem @@ #(/ "|" elem) + / ")"
        :: (λ elems (list (cons 'ELEM-ALT elems)))]
     [/ "(" @ elem + / ")"
        :: (λ elems (list (cons 'ELEM-LIST elems)))]]
  )

(module+ test
  (define bnf-string-1 "
stmt: \"pass\"
")
  (check se/datum?
         (wp*/r bnf-string-1 (bnf-parser->with-surrounding-layout
                              syntactic-bnf-parser))
         (list #'(top-level [arm stmt ":" [alt ((elem () () () "pass" () () ())) ()]])))

  (check se/datum?
         (wp*/r "something: $(in lisp escape)" (bnf-parser->with-surrounding-layout
                                                syntactic-bnf-parser))
         (list #'(top-level [arm something ":" [alt ((elem () () () (in lisp escape) () () ())) ()]])))



  (define bnf-string/stmt "
stmt : \"pass\"
     | expr ; a comment
     | \"{\" stmt + \"}\"
;; another comment
expr : @ $(follow-filter bnumber bnumber)
     | expr \"+\" expr & left
     | expr \"*\" expr & left > \"+\"
     | m1 = expr \"mirror\"
       $(foo)
       & left > \"*\"
/bnumber : (\"0\" | \"1\") +
           :: (λ (elems) (list (apply string-append (syntax->datum elems))))
")

  (define result (whole-parse* (open-input-string bnf-string/stmt)
                               (bnf-parser->with-surrounding-layout
                                syntactic-bnf-parser)))


  ;; This doesn't seem to work, and I'm not sure why.
  ;(print-syntax-width +inf.0)

  (check se/datum?
         (map parse-derivation-result
              (stream->list
               (whole-parse* (open-input-string bnf-string/stmt
                                                ;; use this name to make it easy to compare syntax output when they differ -- it's the same length
                                                "aaaaaaaaaaaaaaaA")
                             (bnf-parser->with-surrounding-layout
                              syntactic-bnf-parser))))
         (list #'(top-level
                  [arm stmt ":"
                       {alt ([elem () () () "pass" () () ()]) ()}
                       {alt ([elem () () () expr () () ()]) ()}
                       {alt
                        ([elem () () () "{" () () ()]
                         [elem () () () stmt () () ("+")]
                         [elem () () () "}" () () ()])
                        ()}]
                  [arm expr ":"
                       {alt ([elem () () ("@")
                                   (follow-filter bnumber bnumber)
                                   () () ()]) ()}
                       {alt
                        ([elem () () () expr () () ()]
                         [elem () () () "+" () () ()]
                         [elem () () () expr () () ()])
                        (["&" "left"])}
                       {alt
                        ([elem () () () expr () () ()]
                         [elem () () () "*" () () ()]
                         [elem () () () expr () () ()])
                        (["&" "left"] [">" "+"])}
                       {alt
                        ([elem ((m1 "=")) () () expr () () ()]
                         [elem () () () "mirror" () () ()]
                         [elem () () () (foo) () () ()])
                        (["&" "left"] [">" "*"])}
                       ]
                  [arm "/" bnumber ":"
                       {alt
                        ([elem () () () (ELEM-ALT [elem () () () "0" () () ()]
                                                  [elem () () () "1" () () ()]
                                                  )
                               () () ("+")])
                        (("::" (λ (elems)
                                 (list (apply string-append
                                              (syntax->datum elems))))))}]
                  )))

  )

