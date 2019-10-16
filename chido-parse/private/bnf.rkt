#lang racket/base

(require
 "scheduler.rkt"
 "readtable-parser.rkt"
 "procedural-combinators.rkt"
 "parameters.rkt"
 racket/stream
 racket/match
 racket/list
 (for-syntax
  racket/base
  syntax/parse
  racket/list
  racket/string
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
              (~or (~optional (~seq #:name name:str))
                   (~optional (~seq #:associativity assoc:expr))
                   (~optional (~seq #:precidence-greater-than pgt:expr))
                   (~optional (~seq #:precidence-less-than plt:expr)))
              ...]
             #:attr parser #f)
    (pattern parser:expr
             #:attr (elem 1) #f
             #:attr name #f
             #:attr assoc #f
             #:attr pgt #f
             #:attr plt #f
             ))
  )

(define-syntax (define-bnf-arm stx)
  ;; TODO - option for whether or not to blacklist literal parts
  (syntax-parse stx
    [(_ arm-name:id
        spec:bnf-arm-alt-spec
        ...
        (~or (~optional (~seq #:layout layout-arg:expr))
             (~optional (~seq #:layout-parsers layout-parsers:expr))
             )
        ...)
     (define/syntax-parse (alt-name/direct ...)
       #'((~? spec.name #f) ...))
     (define/syntax-parse
       ([alt-name/inferred
         [symbol-blacklist ...]
         [prefix-op? postfix-op?]
         assoc
         precidence-gt
         precidence-lt] ...)
       (for/list ([alt (syntax->list #'(spec ...))])
         (syntax-parse alt
           [s:bnf-arm-alt-spec
            (define elems
              (syntax->list #'(~? (s.elem ...) ())))
            (define str-elems/stx
              (filter (syntax-parser [x:str #t]
                                     [else #f])
                      elems))
            (define str-elems (map syntax->datum str-elems/stx))
            (define name (if (null? str-elems)
                             #f
                             (string-join str-elems "_")))
            (define postfix-op?
              (syntax-parse elems
                [(x:id others ...)
                 (datum->syntax #'x
                                (free-identifier=? #'x #'arm-name)
                                #'x)]
                [else #'#f]))
            (define prefix-op?
              (syntax-parse elems
                [(others ... x:id)
                 (datum->syntax #'x
                                (free-identifier=? #'x #'arm-name)
                                #'x)]
                [else #'#f]))
            (define assoc #'(~? s.assoc #f))
            (define precidence-gt #'(~? s.pgt '()))
            (define precidence-lt #'(~? s.plt '()))
            (list (datum->syntax alt name alt)
                  (list str-elems)
                  (list prefix-op? postfix-op?)
                  assoc
                  precidence-gt
                  precidence-lt)])))
     #'(begin
         (define rt1 (set-chido-readtable-symbol-support empty-chido-readtable #f))
         (define layout-arg-use (~? layout-arg 'required))
         (define between-layout-parser
           (match layout-arg-use
             ['required (non-cached-parser-thunk
                         (λ() (chido-readtable->layout+ (arm-name))))]
             ['optional (non-cached-parser-thunk
                         (λ() (chido-readtable->layout* (arm-name))))]
             ['none #f]))
         (define alts (list (~? spec.parser
                                (binding-sequence
                                 spec.elem ...
                                 #:between (if (eq? 'none layout-arg-use)
                                               #f
                                               between-layout-parser)
                                 #:name (or alt-name/direct
                                            alt-name/inferred)))
                            ...))
         (define rt2
           (for/fold ([rt rt1])
                     ([parser alts]
                      [op-spec (list (list prefix-op? postfix-op?) ...)]
                      [a (list assoc ...)]
                      [pgt (list precidence-gt ...)]
                      [plt (list precidence-lt ...)])
             (define op-type
               (match op-spec
                 [(list #f #f) #f]
                 [(list #f #t) 'postfix]
                 [(list #t #f) 'prefix]
                 [(list #t #t) 'infix]))
             ;; TODO - if I add everything as left-recursive-nonterminating, I get an infinite loop.  I'm not sure why.  It's a bad problem...
             (define extension-type
               (if (member op-type '(infix postfix))
                   'left-recursive-nonterminating
                   'nonterminating))
             (extend-chido-readtable
              rt extension-type parser
              #:operator op-type
              #:associativity a
              #:precidence-greater-than pgt
              #:precidence-less-than plt)))
         (define rt3
           (for/fold ([rt rt2])
                     ([parser (~? layout-parsers (list " " "\n" "\t" "\r"))])
             (extend-chido-readtable rt 'layout parser)))
         (define rt4 (chido-readtable-blacklist-symbols
                      rt3
                      (flatten (list (list 'symbol-blacklist ...)
                                     ...))))
         (define arm-name (chido-parse-parameter rt4)))]))






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
  (check-equal? (parser-name
                 (parse-derivation-parser
                  (car (stream->list
                        (parse* (open-input-string "a a b")
                                (chido-readtable->read1 (test1/layout)))))))
                "a_a_b")

  (define-bnf-arm test2
    "n"
    [test2 "+" test2 #:associativity 'left]
    [test2 "*" test2 #:associativity 'left #:precidence-greater-than '("+")]
    [test2 "^" test2 #:associativity 'right #:precidence-greater-than '("*")]
    [test2 "++" #:precidence-less-than '("*") #:precidence-greater-than '("+")]
    ["if" test2 "then" test2 "else" test2 #:precidence-less-than '("+")]
    #:layout 'optional)

  (check-equal? (->results (parse* (open-input-string "n")
                                   (chido-readtable->read1 (test2))))
                '("n"))
  (check-equal? (->results (whole-parse*
                            (open-input-string "n+n+n")
                            test2))
                '((("n" "+" "n") "+" "n")))
  (check-equal? (->results (whole-parse*
                            (open-input-string "n ++")
                            test2))
                '(("n" "++")))
  (check-equal? (->results (whole-parse*
                            (open-input-string "n + n ++")
                            test2))
                '(("n" "+" ("n" "++"))))
  (check-equal? (->results (whole-parse*
                            (open-input-string "n * n ++")
                            test2))
                '((("n" "*" "n") "++")))
  (check-equal? (->results (whole-parse* (open-input-string "n + n * n * n + n")
                                         test2))
                '((("n" "+" (("n" "*" "n") "*" "n")) "+" "n")))

  (check-equal? (->results (whole-parse*
                            (open-input-string "if n then n else n")
                            test2))
                '(("if" "n" "then" "n" "else" "n")))
  ;; Optional space around operators is not as nice as required space.
  (check-equal? (->results (whole-parse*
                            (open-input-string "ifnthennelsen")
                            test2))
                '(("if" "n" "then" "n" "else" "n")))
  (check-equal? (->results (whole-parse*
                            (open-input-string "n + if n then n else n + n")
                            test2))
                '(("n" "+" ("if" "n" "then" "n" "else" ("n" "+" "n")))))

  )
