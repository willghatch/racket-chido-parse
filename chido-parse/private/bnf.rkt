#lang racket/base

(provide
 define-bnf-arm
 define-bnf
 )

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

;; TODO - repetition (kleene star/plus and generalization)
;; TODO - sub-sequences (eg. so a repetition can be over a sequence)
;; TODO - bnf extension forms -- something like (extend-bnf bnf-name arm-name new-parser/sequence-spec)

(define bnf-default-layout-requirement 'optional)
(define bnf-default-layout-parsers '(" " "\t" "\r" "\n"))

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

(define-syntax (layout-inherit stx)
  (raise-syntax-error 'layout-inherit
                      "only exists for use as a hidden keyword of define-bnf-arm"
                      stx))
(define-syntax (layout-parsers-inherit stx)
  (raise-syntax-error 'layout-parsers-inherit
                      "only exists for use as a hidden keyword of define-bnf-arm"
                      stx))

(define-syntax (extend-bnf-arm stx)
  (syntax-parse stx
    [(_ arm:expr spec:bnf-arm-alt-spec ...+)
     #|
     TODO - the BNF arm needs to store its #:layout spec...
          - This implies that readtables need to store extra data?!
     Or...
     • readtables have a hash table of extra user data, BNF stores stuff here and has a default value for a readtable that hasn't set it.
     • !!! Maybe this means I just always add the between parsers, and they check the readtable field to tell whether they use the layout parser or an epsilon parser.
     • or readtables could have a hash-table specific field
     • or maybe I always just give the keyword... I don't like that.

     So my favorite idea is for readtables to have a hash table of extra data, and have BNF have keys to store the layout inheritance policy.
     |#
     (raise-syntax-error 'TODO)]))

(define-syntax (define-bnf-arm stx)
  ;; TODO - option for whether or not to blacklist literal parts
  (syntax-parse stx
    [(_ arm-name:id
        (~or (~optional (~seq #:layout layout-arg:expr))
             (~optional (~seq #:layout-parsers layout-parsers:expr))
             (~optional (~seq (~literal layout-inherit)
                              layout/inherit:expr))
             (~optional (~seq (~literal layout-parsers-inherit)
                              layout-parsers/inherit:expr))
             )
        ...
        spec:bnf-arm-alt-spec
        ...+)
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
         (define layout-arg-use (~? layout-arg bnf-default-layout-requirement))
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
                     ([parser (~? layout-parsers bnf-default-layout-parsers)])
             (extend-chido-readtable rt 'layout parser)))
         (define rt4 (chido-readtable-blacklist-symbols
                      rt3
                      (flatten (list (list 'symbol-blacklist ...)
                                     ...))))
         (define arm-name (chido-parse-parameter rt4)))]))






(module+ test
  (define (->results ds)
    (map parse-derivation-result (stream->list ds)))

  (define number-parser
    (proc-parser
     (λ (port)
       (define r (read port))
       (define-values (line col pos) (port-next-location port))
       (if (number? r)
           (make-parse-derivation r #:end pos)
           (error "not a number")))))

  (define-bnf-arm test1
    #:layout 'none
    ["a" [#:ignore #t "b"] "c"]
    ["a" "a" "b"])
  (define-bnf-arm test1/layout
    #:layout 'required
    ["a" [#:ignore #t "b"] "c" #:name "test-name-1"]
    ["a" "a" "b"])
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
    #:layout 'optional
    "n"
    [test2 "+" test2 #:associativity 'left]
    [test2 "*" test2 #:associativity 'left #:precidence-greater-than '("+")]
    [test2 "^" test2 #:associativity 'right #:precidence-greater-than '("*")]
    [test2 "++" #:precidence-less-than '("*") #:precidence-greater-than '("+")]
    ["if" test2 "then" test2 "else" test2 #:precidence-less-than '("+")])

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

(struct bnf-parser
  (main-arm-key arm-hash layout-parsers layout-alt)
  #:property prop:custom-parser
  (λ (self) (hash-ref (bnf-parser-arm-hash self)
                      (bnf-parser-main-arm-key self))))

(define (bnf-parser-ref bnf-parser key
                        [default (λ () (error 'bnf-parser-ref
                                              "key not found: ~v"
                                              key))])
  (hash-ref (bnf-parser-arm-hash bnf-parser) key default))


(define-syntax (define-bnf stx)
  (syntax-parse stx
    [(_ name:id
        (~or (~optional (~seq #:layout-parsers layout-parsers:expr))
             (~optional (~seq #:layout layout-arg:expr))
             (~optional (~seq #:main-arm main-arm-arg:id))
             )
        ...
        [arm-name:id arm-spec/kw ...]
        ...)
     (define/syntax-parse main-arm:id
       (or (attribute main-arm-arg)
           (first (syntax->list #'(arm-name ...)))))

     #'(define name
         (let ()
           (define layout/inherit
             (~? layout-arg bnf-default-layout-requirement))
           (define layout-parsers/use
             ;; TODO - maybe I should keep this as a separate thing that can be extended all together?
             (~? layout-parsers bnf-default-layout-parsers))
           (define layout-alt
             (make-alt-parser (format "~a_master-layout-parser" 'name)
                              layout-parsers/use))
           (define master-layout-parser
             (proc-parser #:name (format "~a_master-layout-parser" 'name)
                          (λ (port)
                            (parse* port
                                    (bnf-parser-layout-alt
                                     (inner-bnf-name))))))

           ;; to avoid "arm-name used before initialization" errors, do some indirection
           (define (get-current-bnf)
             inner-bnf-name)
           (define-syntax (arm-name stx)
             (syntax-parse stx
               [x:id #'(λ () (bnf-parser-ref ((get-current-bnf))
                                             'arm-name))]
               [else (raise-syntax-error
                      'define-bnf
                      "arm names can currently only be used as identifiers"
                      stx)]))
           ...
           (define inner-bnf-name
             (chido-parse-parameter
              (let ([arm-name (let ()
                                (define-bnf-arm arm-name
                                  layout-parsers-inherit (list master-layout-parser)
                                  layout-inherit layout/inherit
                                  arm-spec/kw ...)
                                arm-name)]
                    ...)
                (bnf-parser 'main-arm
                            (hash (~@ 'arm-name arm-name) ...)
                            layout-parsers/use
                            layout-alt))))
           inner-bnf-name))]))

(define-syntax (extend-bnf stx)
  (syntax-parse stx
    [(_ bnf arm-name new-parser/sequence-spec ...+)
     #'(let ([bnf* bnf]
             [arm-name* arm-name])
         ;; TODO - better error message on this hash-ref
         (struct-copy
          bnf bnf*
          [arm-hash (hash-set bnf* arm-name*
                              (extend-bnf-arm (hash-ref bnf* arm-name*)
                                              new-parser/sequence-spec ...))]))]))


(module+ test
  (define-bnf bnf-test-1
    [statement ["pass"]
               ["for" id "in" expression "do" statement]
               ["{" statement "}" #:name "block"]
               expression]
    [expression number-parser
                id
                [expression "+" expression #:associativity 'left]
                [expression "*" expression
                            #:associativity 'right
                            #:precidence-greater-than '("+")]]
    [id "x"])

  (check-equal? (->results (whole-parse*
                            (open-input-string "pass")
                            bnf-test-1))
                '(("pass")))
  (check-equal? (->results (whole-parse*
                            (open-input-string "5 + 67 * 3")
                            bnf-test-1))
                '((5 "+" (67 "*" 3))))
  (check-equal? (->results (whole-parse*
                            (open-input-string "for x in 27 + 13 do { 555 * x }")
                            bnf-test-1))
                '(("for" "x" "in" (27 "+" 13) "do" ("{" (555 "*" "x") "}"))))
  )
