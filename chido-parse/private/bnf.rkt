#lang racket/base

(require
 "scheduler.rkt"
 "readtable-parser.rkt"
 "procedural-combinators.rkt"
 "parameters.rkt"
 racket/stream
 racket/match
 (for-syntax
  racket/base
  syntax/parse
  ))
(module+ test
  (require
   rackunit
   ))


#|
* top-level parser function
** need to specify (or assume) which sub-parser is the entry point
** need to specify (or assume) layout parsers
** need to specify (or assume) whether auto layout insertion should be used

* sub parsers / nonterminals (eg. Expression, Statement, etc alts)
** maybe specify custom layout parsers and auto-insertion for each
** specify whether symbols are enabled (default off)

* bottom-level parsers (IE alts within Expression, Statement, etc)
** need a way to name the parser, with some sensible default available
** needs to be a sequence with easy */+ notation for repetition (where the repetition will honor layout-insertion)
*** also needs grouping with repetition -- eg. ["prefix" (expr type) * "postfix"].  Maybe this is too much -- a group could just be another parser...
** need a way to specify filters
** need a way to bind variables in sequence
** need a way to tell when something is a left-or-right recursive operator
** need a way to specify operator precidence and assoc
** need a way to specify return value of whole form
** need a way to declare that sub-forms have their results ignored or spliced
*** IE there should be a default return value constructor that makes a syntax object with things ignored or spliced, but then a function that can take the syntax object components and do something.
** maybe override for inherited things, like auto-layout-insertion
** optional spec for terminating, nonterminating, etc -- default to left-recursive-nonterminating?

|#


(begin-for-syntax
  (define-syntax-class bnf-arm-alt-spec
    ;; TODO - name
    ;; TODO - detect left/right recursion (IE operator-ness)
    (pattern [elem:binding-sequence-elem
              ...+
              (~or (~optional (~seq #:name name:str)))
              ...]
             #:attr parser #f)
    (pattern parser:expr
             #:attr (elem 1) #f
             #:attr name #f
             ))
  )

(define-syntax (define-bnf-arm stx)
  ;; TODO - declare layout parsers
  (syntax-parse stx
    [(_ arm-name:id
        spec:bnf-arm-alt-spec
        ...
        (~or (~optional (~seq #:layout layout-arg:expr))
             (~optional (~seq #:layout-parsers layout-parsers:expr))
             )
        ...)
     #'(begin
         (define rt1 (set-chido-readtable-symbol-support empty-chido-readtable #f))
         (define layout-arg-use (~? layout-arg 'required))
         (define (between-layout-parser)
           (match layout-arg-use
             ['required (chido-readtable->layout+ (arm-name))]
             ['optional (chido-readtable->layout* (arm-name))]
             ['none #f]))
         (define alts (list (~? spec.parser
                                (binding-sequence
                                 spec.elem ...
                                 #:between (if (eq? 'none layout-arg-use)
                                               #f
                                               between-layout-parser)
                                 #:name (~? spec.name #f)))
                            ...))
         (define rt2
           (for/fold ([rt rt1])
                     ([parser alts])
             ;; TODO - properly detect operators, precidence, assoc, etc
             (extend-chido-readtable rt 'left-recursive-nonterminating parser)))
         (define rt3
           (for/fold ([rt rt2])
                     ([parser (~? layout-parsers (list " " "\n" "\t" "\r"))])
             (extend-chido-readtable rt 'layout parser)))
         (define arm-name (chido-parse-parameter rt3)))]))






(module+ test
  (define (->results ds)
    (map parse-derivation-result (stream->list ds)))

  (define-bnf-arm test1
    ["a" [#:ignore #t "b"] "c"]
    ["a" "a" "b"]
    #:layout 'none)
  (define-bnf-arm test1/layout
    ["a" [#:ignore #t "b"] "c" #:name "test-name-1"]
    ["a" "a" "b"]
    #:layout 'required)
  (check-equal? (->results (parse* (open-input-string "abc")
                                   (chido-readtable->read1 (test1))))
                '(("a" "c")))
  (check-equal? (->results (parse* (open-input-string "aab")
                                   (chido-readtable->read1 (test1))))
                '(("a" "a" "b")))
  (check-equal? (->results (parse* (open-input-string "abbc")
                                   (chido-readtable->read1 (test1))))
                '())

  (check-equal? (->results (parse* (open-input-string "a b c")
                                   (chido-readtable->read1 (test1/layout))))
                '(("a" "c")))
  (check-equal? (->results (parse* (open-input-string "a a b")
                                   (chido-readtable->read1 (test1/layout))))
                '(("a" "a" "b")))
  (check-equal? (->results (parse* (open-input-string "a b b c")
                                   (chido-readtable->read1 (test1/layout))))
                '())

  ;; check that naming works
  (check-equal? (parser-name
                 (parse-derivation-parser
                  (car (stream->list
                        (parse* (open-input-string "a b c")
                                (chido-readtable->read1 (test1/layout)))))))
                "test-name-1")

  )
