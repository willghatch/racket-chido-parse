#lang racket/base

(provide
 define-bnf-arm
 define-bnf
 define-bnf/quick

 readtable-extend-as-bnf-arm
 extend-bnf

 #|
 TODO
 define-bnf-arm/quick
 readtable-extend-as-bnf-arm/quick
 extend-bnf/quick
 |#

 bnf-parser->with-surrounding-layout
 bnf-parser->arm-parser
 )

(require
 "core.rkt"
 "readtable-parser.rkt"
 "procedural-combinators.rkt"
 "binding-sequence.rkt"
 "parameters.rkt"
 racket/stream
 racket/match
 racket/list
 racket/dict
 (for-syntax
  racket/base
  syntax/parse
  racket/list
  racket/string
  ))
(module+ test
  (require
   rackunit
   "test-util-3.rkt"
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


(define bnf-default-layout-requirement 'optional)
(define bnf-default-layout-parsers '(" " "\t" "\r" "\n"))
(define bnf-layout-mode-key (gensym 'bnf-layout-mode-key))
(define current-bnf (chido-parse-parameter #f))

(define bnf-inserted-layout-parser
  (proc-parser
   (λ (port)
     (define real-parser
       (match (dict-ref (current-chido-readtable)
                        bnf-layout-mode-key
                        bnf-default-layout-requirement)
         ['required (chido-readtable->layout+ (current-chido-readtable))]
         ['optional (chido-readtable->layout* (current-chido-readtable))]
         ['none epsilon-parser]))
     (parse* port real-parser))))

(begin-for-syntax
  (define-syntax-class bnf-arm-alt-spec
    (pattern [elem:binding-sequence-elem
              ...+
              (~or (~optional (~seq #:name name:str))
                   (~optional (~seq #:associativity assoc:expr))
                   (~optional (~seq #:precidence-greater-than pgt:expr))
                   (~optional (~seq #:precidence-less-than plt:expr))
                   (~optional (~seq #:result/stx result/stx:expr))
                   (~optional (~seq #:result/bare result/bare:expr))
                   )
              ...]))
  )

(define-syntax (result/bare-inherit stx)
  (raise-syntax-error 'result/bare-inherit
                      "only exists for use as a hidden keyword of define-bnf-arm"
                      stx))
(define-syntax (result/stx-inherit stx)
  (raise-syntax-error 'result/stx-inherit
                      "only exists for use as a hidden keyword of define-bnf-arm"
                      stx))
(define-syntax (layout-inherit stx)
  (raise-syntax-error 'layout-inherit
                      "only exists for use as a hidden keyword of define-bnf-arm"
                      stx))
(define-syntax (layout-parsers-inherit stx)
  (raise-syntax-error 'layout-parsers-inherit
                      "only exists for use as a hidden keyword of define-bnf-arm"
                      stx))

(define-syntax (define-bnf-arm stx)
  ;; TODO - option for whether or not to blacklist literal parts
  (syntax-parse stx
    [(_ arm-name:id
        (~or (~optional (~seq #:layout layout-arg:expr))
             (~optional (~seq #:layout-parsers layout-parsers:expr))
             (~optional (~seq #:ignore-arm-name? ignore-arm-name/given:expr))
             (~optional (~seq #:result/stx arm-result/stx:expr))
             (~optional (~seq #:result/bare arm-result/bare:expr))
             (~optional (~seq (~literal result/stx-inherit)
                              result/stx/inherit:expr))
             (~optional (~seq (~literal result/bare-inherit)
                              result/bare/inherit:expr))
             (~optional (~seq (~literal layout-inherit)
                              layout/inherit:expr))
             (~optional (~seq (~literal layout-parsers-inherit)
                              layout-parsers/inherit:expr))
             )
        ...
        spec:bnf-arm-alt-spec
        ...+)
     #'(begin
         (define ignore-arm-name-use (~? ignore-arm-name/given #f))
         (define layout-arg-use (~? layout-arg
                                    (~? layout/inherit
                                        bnf-default-layout-requirement)))
         (define given-result/bare? (~? arm-result/bare #f))
         (define given-result/stx? (~? arm-result/stx #f))
         (define inherit-result/bare? (~? result/bare/inherit #f))
         (define inherit-result/stx? (~? result/stx/inherit #f))
         (define use-result/bare? (or given-result/bare?
                                      (and inherit-result/bare?
                                           (not given-result/stx?))))
         (define use-result/stx? (or given-result/stx?
                                     (and inherit-result/stx?
                                          (not given-result/bare?))))
         (define rt1
           (dict-set
            (set-chido-readtable-name
             (set-chido-readtable-symbol-support empty-chido-readtable #f)
             'arm-name)
            bnf-layout-mode-key
            layout-arg-use))

         (define rt2
           (for/fold ([rt rt1])
                     ([parser (~? layout-parsers
                                  (~? layout-parsers/inherit
                                      bnf-default-layout-parsers))])
             (extend-chido-readtable 'terminating-layout parser rt)))

         (define rt3 (readtable-extend-as-bnf-arm
                      rt2
                      #:ignore-arm-name? ignore-arm-name-use
                      #:result/stx use-result/stx?
                      #:result/bare use-result/bare?
                      #:arm-name arm-name
                      spec ...))
         (define arm-name rt3))]))


(define-syntax (readtable-extend-as-bnf-arm stx)
  (syntax-parse stx
    [(_ arm:expr
        (~or (~optional (~seq #:arm-name arm-name:id))
             (~optional (~seq #:ignore-arm-name? ignore-arm-name?/given:expr))
             (~optional (~seq #:result/stx default-result/stx:expr))
             (~optional (~seq #:result/bare default-result/bare:expr))
             )
        ...
        spec:bnf-arm-alt-spec ...)
     (define/syntax-parse (alt-name/direct ...)
       #'((~? spec.name #f) ...))
     (define/syntax-parse
       ([alt-name/inferred
         [symbol-blacklist ...]
         [prefix-op? postfix-op?]
         assoc
         precidence-gt
         precidence-lt
         result/stx
         result/bare] ...)
       (for/list ([alt (syntax->list #'(spec ...))])
         (syntax-parse alt
           [s:bnf-arm-alt-spec
            (define parsers
              (syntax->list (syntax-parse #'(~? (s.elem ...) ())
                              [(e:binding-sequence-elem ...)
                               #'(e.parser ...)])))
            (define str-elems/stx
              (filter (syntax-parser [x:str #t]
                                     [else #f])
                      parsers))
            (define str-elems (map syntax->datum str-elems/stx))
            (define name (if (null? str-elems)
                             #'(format "alt-for:_~a" (parser-name arm))
                             (string-join str-elems "_")))
            (define postfix-op?
              (syntax-parse parsers
                [(x:id others ...)
                 (datum->syntax #'x
                                (or (free-identifier=? #'x #'arm-name)
                                    (free-identifier=? #'x #'current-chido-readtable))
                                #'x)]
                [else #'#f]))
            (define prefix-op?
              (syntax-parse parsers
                [(others ... x:id)
                 (datum->syntax #'x
                                (or (free-identifier=? #'x #'arm-name)
                                    (free-identifier=? #'x #'current-chido-readtable))
                                #'x)]
                [else #'#f]))
            (define assoc #'(~? s.assoc #f))
            (define precidence-gt #'(~? s.pgt '()))
            (define precidence-lt #'(~? s.plt '()))
            (define result/stx #'(~? s.result/stx #f))
            (define result/bare #'(~? s.result/bare #f))
            (list (datum->syntax alt name alt)
                  (list str-elems)
                  (list prefix-op? postfix-op?)
                  assoc
                  precidence-gt
                  precidence-lt
                  result/stx
                  result/bare)])))
     #'(let-syntax ([arm-name (syntax-parser [_ #'current-chido-readtable])])
         (let* ([rt/start arm]
                [use-result/stx? (~? default-result/stx #f)]
                [use-result/bare? (~? default-result/bare #f)]
                [default-use-stx? (not use-result/bare?)]
                [ignore-arm-name? (~? ignore-arm-name?/given #f)]
                ;;;;;;;;;; TODO - aoeu - I need to inject the arm name into the list even when there is a result-specific handler, UNLESS ignore-arm-name is true.
                [alts (list (let* ([alt-specific-result/stx result/stx]
                                   [alt-specific-result/bare result/bare]
                                   [arm-specific-result/pre-name-add
                                    (or alt-specific-result/stx
                                        alt-specific-result/bare
                                        ;; TODO - should I allow use-result/stx or use-result/bare to be a procedure that builds the result?  As a default it would be hard to make a good procedure...
                                        (λ elems elems))]
                                   [arm-specific-result
                                    (if (or alt-specific-result/stx
                                            alt-specific-result/bare
                                            ignore-arm-name?)
                                        arm-specific-result/pre-name-add
                                        (λ args
                                          (apply arm-specific-result/pre-name-add
                                                 (cons 'arm-name args) )))]
                                   [arm-specific-use-stx?
                                    (or alt-specific-result/stx
                                        (and (not alt-specific-result/bare)
                                             default-use-stx?))])
                              (binding-sequence
                               spec.elem ...
                               #:result/stx (and arm-specific-use-stx?
                                                 arm-specific-result)
                               #:result/bare (and (not arm-specific-use-stx?)
                                                  arm-specific-result)
                               #:between bnf-inserted-layout-parser
                               #:name (or alt-name/direct
                                          alt-name/inferred)))
                            ...)]
                [rt/extended
                 (for/fold ([rt rt/start])
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
                   (define extension-type
                     ;; TODO - I should have an option for symbol support, and if symbols are supported divide the parsers.  But when symbol support is undesired, it would be better to make everything either left-recursive-nonterminating or make everything NOT left-recursive-nonterminating.  If the user adds a custom procedure NOT built using the built-in binding-sequence of define-bnf-arm, and makes it left-recursive but doesn't mark it left-recursive-nonterminating, it WON'T see the results of things that ARE marked left-recursive-nonterminating.  If symbol support is on, then users need to take care to mark alternates explicitly to affect symbol parsing.
                     'left-recursive-nonterminating
                     #;(if (member op-type '(infix postfix))
                           'left-recursive-nonterminating
                           'nonterminating))
                   (extend-chido-readtable
                    extension-type parser
                    #:operator op-type
                    #:associativity a
                    #:precidence-greater-than pgt
                    #:precidence-less-than plt
                    rt))]
                [rt/blacklisted (chido-readtable-blacklist-symbols
                                 (flatten (list (list 'symbol-blacklist ...)
                                                ...))
                                 rt/extended)])
           rt/blacklisted))]))






(module+ test
  (define number-parser
    (proc-parser
     (λ (port)
       (define r
         (with-handlers ([(λ(e)#t)(λ(e)(exn->failure e))])
           (read port)))
       (define-values (line col pos) (port-next-location port))
       (if (number? r)
           (make-parse-derivation r #:end pos)
           (make-parse-failure #:message "not a number")))))

  (define-bnf-arm test1
    #:ignore-arm-name? #t
    #:result/bare #t
    #:layout 'none
    ["a" [: #:ignore #t "b"] "c"]
    ["a" "a" "b"])
  (define-bnf-arm test1/layout
    #:ignore-arm-name? #t
    #:result/bare #t
    #:layout 'required
    ["a" [: #:ignore #t "b"] "c" #:name "test-name-1"]
    ["a" "a" "b"])
  (check-equal? (p*/r "abc"
                      (chido-readtable->read1 test1))
                '(("a" "c")))
  (check-equal? (p*/r  "aab"
                       (chido-readtable->read1 test1))
                '(("a" "a" "b")))
  (check-equal? (p*/r "abbc"
                      (chido-readtable->read1 test1))
                '())

  (check-equal? (p*/r "a b c"
                      (chido-readtable->read1 test1/layout))
                '(("a" "c")))
  (check-equal? (p*/r "a a b"
                      (chido-readtable->read1 test1/layout))
                '(("a" "a" "b")))
  (check-equal? (p*/r "a b b c"
                      (chido-readtable->read1 test1/layout))
                '())

  ;; check that naming works
  (check-equal? (parser-name
                 (parse-derivation-parser
                  (car (stream->list
                        (parse* (open-input-string "a b c")
                                (chido-readtable->read1 test1/layout))))))
                "test-name-1")
  (check-equal? (parser-name
                 (parse-derivation-parser
                  (car (stream->list
                        (parse* (open-input-string "a a b")
                                (chido-readtable->read1 test1/layout))))))
                "a_a_b")

  (define-bnf-arm test2
    #:layout 'optional
    #:result/bare #t
    #:ignore-arm-name? #t
    ["n"]
    [test2 "+" test2 #:associativity 'left]
    [test2 "*" test2 #:associativity 'left #:precidence-greater-than "+"]
    [test2 "^" test2 #:associativity 'right #:precidence-greater-than "*"]
    [test2 "++" #:precidence-less-than '("*") #:precidence-greater-than "+"]
    ["if" test2 "then" test2 "else" test2 #:precidence-less-than "+"])

  (check-equal? (p*/r "n"
                      (chido-readtable->read1 test2))
                '(("n")))
  (check-equal? (wp*/r "n+n+n" test2)
                '(((("n") "+" ("n")) "+" ("n"))))
  (check-equal? (wp*/r "n ++" test2)
                '((("n") "++")))
  (check-equal? (wp*/r "n + n ++" test2)
                '((("n") "+" (("n") "++"))))
  (check-equal? (wp*/r "n * n ++" test2)
                '(((("n") "*" ("n")) "++")))
  (check-equal? (wp*/r "n + n * n * n + n" test2)
                '(((("n") "+" ((("n") "*" ("n")) "*" ("n"))) "+" ("n"))))

  (check-equal? (wp*/r "if n then n else n" test2)
                '(("if" ("n") "then" ("n") "else" ("n"))))
  ;; Optional space around operators is not as nice as required space.
  (check-equal? (wp*/r "ifnthennelsen" test2)
                '(("if" ("n") "then" ("n") "else" ("n"))))
  (check-equal? (wp*/r "n + if n then n else n + n" test2)
                '((("n") "+" ("if" ("n") "then" ("n") "else" (("n") "+" ("n"))))))

  )

(struct bnf-parser
  (main-arm-key arm-hash layout-parsers layout-alt arm-name-ignore-hash)
  #:property prop:custom-parser
  (λ (self)
    (proc-parser
     (λ (port)
       (chido-parse-parameterize
        ([current-bnf self])
        (parse* port
                (hash-ref (bnf-parser-arm-hash self)
                          (bnf-parser-main-arm-key self))))))))

(define (bnf-parser->arm-parser bnf arm-name)
  (define arm (dict-ref (bnf-parser-arm-hash bnf) arm-name))
  (proc-parser
   (λ (port)
     (chido-parse-parameterize ([current-bnf bnf])
                               (parse* port arm)))))

(define (bnf-parser->with-surrounding-layout bnf)
  (sequence (kleene-star (bnf-parser-layout-alt bnf)
                         ;; TODO - figure out what to best do here
                         ;#:greedy? #t
                         )
            bnf
            (kleene-star (bnf-parser-layout-alt bnf)
                         ;#:greedy? #t
                         )
            #:result/stx (λ (l bnf r) bnf)))

(define (bnf-parser-ref bnf-parser key
                        [default (λ () (error 'bnf-parser-ref
                                              "key not found: ~v"
                                              key))])
  (hash-ref (bnf-parser-arm-hash bnf-parser) key default))

(define bnf-layout-parser
  (proc-parser #:name "bnf-layout-parser"
               (λ (port)
                 (parse* port
                         (bnf-parser-layout-alt
                          (current-bnf))))))

(define-syntax (define-bnf stx)
  (syntax-parse stx
    [(_ name:id
        (~or (~optional (~seq #:layout-parsers layout-parsers:expr))
             (~optional (~seq #:layout layout-arg:expr))
             (~optional (~seq #:result/stx result/stx:expr))
             (~optional (~seq #:result/bare result/bare:expr))
             (~optional (~seq #:main-arm main-arm-arg:id))
             )
        ...
        [arm-name:id
         (~or (~optional (~seq #:ignore-arm-name? ignore-arm-name?/given:expr))
              (~optional (~seq #:layout layout-for-arm?/given:expr)))
         ...
         arm-spec/kw ...]
        ...)
     (define/syntax-parse main-arm:id
       (or (attribute main-arm-arg)
           (first (syntax->list #'(arm-name ...)))))
     (define/syntax-parse (ignore-arm-name ...)
       (generate-temporaries #'(arm-name ...)))

     #'(define name
         (let ()
           (define layout/inherit
             (~? layout-arg bnf-default-layout-requirement))
           (define result/stx/inherit (~? result/stx #f))
           (define result/bare/inherit (~? result/bare #f))
           (define layout-parsers/use
             ;; TODO - maybe I should keep this as a separate thing that can be extended all together?
             (~? layout-parsers bnf-default-layout-parsers))
           (define layout-alt
             (make-alt-parser (format "~a_master-layout-parser" 'name)
                              layout-parsers/use))

           ;; to avoid "arm-name used before initialization" errors, do some indirection
           (define-syntax (arm-name stx)
             (syntax-parse stx
               [x:id #'(proc-parser
                        (λ (port)
                          (parse* port (bnf-parser-ref (current-bnf)
                                                       'arm-name))))]
               [else (raise-syntax-error
                      'define-bnf
                      "arm names can currently only be used as identifiers"
                      stx)]))
           ...
           (define ignore-arm-name (~? ignore-arm-name?/given #f))
           ...
           (define inner-bnf-name
             (let ([arm-name (let ()
                               (define-bnf-arm arm-name
                                 #:ignore-arm-name? ignore-arm-name
                                 (~? (~@ #:layout layout-for-arm?/given))
                                 layout-parsers-inherit (list bnf-layout-parser)
                                 layout-inherit layout/inherit
                                 result/bare-inherit result/bare/inherit
                                 result/stx-inherit result/stx/inherit
                                 arm-spec/kw ...)
                               arm-name)]
                   ...)
               (bnf-parser 'main-arm
                           (hash (~@ 'arm-name arm-name) ...)
                           layout-parsers/use
                           layout-alt
                           (hash (~@ 'arm-name ignore-arm-name) ...))))
           inner-bnf-name))]))

(define-for-syntax get-current-bnf-arm
  (syntax-parser [x:id #'(proc-parser
                          (λ (port)
                            (parse* port
                                    (bnf-parser-ref (current-bnf) 'x))))]))


(define-syntax (extend-bnf stx)
  (syntax-parse stx
    [(_ bnf
        (~or (~optional (~seq #:result/stx result/stx:expr))
             (~optional (~seq #:result/bare result/bare:expr)))
        ...
        [arm-name:id new-parser/sequence-spec ...] ...+)
     #'(let* ([bnf* bnf]
              [result/stx-given (~? result/stx #f)]
              [result/bare-given (~? result/bare #f)])
         (let-syntax ([arm-name get-current-bnf-arm] ...)
           (extend-bnf-helper bnf*
                              (result/stx-given result/bare-given)
                              ()
                              [arm-name new-parser/sequence-spec ...] ...)))]))
(define-syntax (extend-bnf-helper stx)
  (syntax-parse stx
    [(_ bnf-ref:id
        (result/stx result/bare)
        (arm-done:id ...)
        [arm-for-this-pass:id spec-for-this-pass ...]
        [arm-later spec-later ...] ...)
     #'(let ([new-bnf-id
              (struct-copy
               bnf-parser bnf-ref
               [arm-hash (hash-set
                          (bnf-parser-arm-hash bnf-ref)
                          'arm-for-this-pass
                          (readtable-extend-as-bnf-arm
                           (hash-ref (bnf-parser-arm-hash bnf-ref)
                                     'arm-for-this-pass
                                     (λ() (error
                                           'extend-bnf
                                           "adding new BNF arms not yet supported")))
                           #:arm-name arm-for-this-pass
                           #:result/stx result/stx
                           #:result/bare result/bare
                           spec-for-this-pass ...))])])
         (extend-bnf-helper new-bnf-id
                            (result/stx result/bare)
                            (arm-done ... arm-for-this-pass)
                            [arm-later spec-later ...] ...))]
    [(_ bnf-ref:id (result/stx result/bare) (arm-done:id ...)) #'bnf-ref]))


(module+ test
  (define-bnf bnf-test-1
    #:result/bare #t
    [statement ["pass"]
               ["for" id "in" expression "do" statement]
               ["{" [: #:repeat-min 0 #:splice 1 statement] "}" #:name "block"]
               [expression]]
    [expression [number-parser]
                [id]
                [expression "+" expression #:associativity 'left]
                [expression "*" expression
                            #:associativity 'right
                            #:precidence-greater-than '("+")]]
    [id ["x"]])

  (check-equal? (wp*/r "pass" bnf-test-1)
                '((statement "pass")))
  (check-equal? (wp*/r "5 + 67 * 3" bnf-test-1)
                '((statement (expression (expression 5) "+"
                                         (expression (expression 67)
                                                     "*"
                                                     (expression 3))))))
  (check-equal? (wp*/r "for x in 27 + 13 do { 555 * x }" bnf-test-1)
                '((statement "for" (id "x") "in"
                             (expression (expression 27) "+" (expression 13))
                             "do" (statement "{" (statement (expression
                                                             (expression 555)
                                                             "*"
                                                             (expression (id "x"))))
                                             "}"))))
  (check-equal? (wp*/r "{ 5 6 7 }" bnf-test-1)
                '((statement "{"
                             (statement (expression 5))
                             (statement (expression 6))
                             (statement (expression 7))
                             "}")))

  (define bnf-test-2
    (extend-bnf
     bnf-test-1
     #:result/bare #t
     [statement ["break"]
                [id ":=" expression]]
     [expression ["let" id "=" expression "in" expression
                        #:precidence-greater-than '("*")]]
     [id]))

  (check-equal? (wp*/r "{ pass break 2 + let x = 5 in 2 + 2 }" bnf-test-2)
                '((statement "{"
                             (statement "pass")
                             (statement "break")
                             (statement (expression (expression
                                                     (expression 2)
                                                     "+"
                                                     (expression "let"
                                                                 (id "x")
                                                                 "="
                                                                 (expression 5)
                                                                 "in"
                                                                 (expression 2)))
                                                    "+"
                                                    (expression 2)))
                             "}")))

  )


(begin-for-syntax
  (define-syntax-class ats
    (pattern x:id
             #:when (regexp-match "@+" (symbol->string (syntax->datum #'x)))
             #:attr number (datum->syntax #'here
                                          (string-length
                                           (symbol->string (syntax->datum #'x))))))
  (define-syntax-class quick-subsequence
    (pattern (~and whole-stx #(elem:binding-sequence-elem/quick ...))
             #:attr nonquick (syntax/loc
                                 #'whole-stx
                                 (binding-sequence
                                  elem.nonquick ...
                                  ;; TODO - #:result/stx/bare
                                  ;; TODO - #:name
                                  #:between bnf-inserted-layout-parser
                                  ))))
  (define-syntax-class unspecial-expr
    (pattern (~and (~not (~or (~datum =)
                              (~datum /)
                              (~datum %)
                              (~datum *)
                              (~datum +)
                              (~datum ?)
                              (~datum &)
                              (~datum <)
                              (~datum >)
                              (~datum ::)
                              (~datum ||)
                              at-seq:ats))
                   parser-given:expr)))
  (define-syntax-class bnf-inner-elem-alt
    (pattern ((~datum ||) elem:binding-sequence-elem/quick ...+)
             #:attr nonquick
             #'(make-alt-parser
                "TODO-name_bnf-inner-elem-alt"
                (list (binding-sequence elem.nonquick #:splice 1)
                      ...))))
  (define-splicing-syntax-class binding-sequence-elem/quick
    (pattern (~seq
              (~or (~optional (~seq name:id (~datum =)))
                   (~optional (~and ignore-given (~datum /)))
                   (~optional splice-given:ats))
              ...
              (~or subseq:quick-subsequence
                   inner-alt:bnf-inner-elem-alt
                   parser-given:unspecial-expr)
              (~or (~optional (~and star (~datum *)))
                   (~optional (~and plus (~datum +)))
                   (~optional (~and question (~datum ?)))
                   ))
             #:attr ignore (if (attribute ignore-given)
                               #'#t
                               #'#f)
             #:attr nonquick #'[:
                                (~? subseq.nonquick
                                    (~? inner-alt.nonquick
                                        parser-given))
                                (~? (~@ #:bind name))
                                #:ignore ignore
                                #:splice (~? splice-given.number #f)
                                ;; TODO - figure this out
                                ;#:repeat-greedy? #t
                                #:repeat-min (cond [(~? 'star #f) 0]
                                                   [(~? 'plus #f) 1]
                                                   [else #f])
                                #:repeat-max (cond [(~? 'question #f) 1]
                                                   [else #f])]))
  (define-syntax-class bnf-arm-alt-spec/quick
    (pattern [elem:binding-sequence-elem/quick
              ...+
              (~or (~optional (~seq (~datum &) assoc:expr))
                   (~optional (~seq (~datum >) pgt:expr))
                   (~optional (~seq (~datum <) plt:expr))
                   (~optional (~seq (~datum ::) result:expr))
                   )
              ...]
             #:attr nonquick #'[elem.nonquick
                                ...
                                (~? (~@ #:associativity assoc))
                                (~? (~@ #:precidence-greater-than pgt))
                                (~? (~@ #:precidence-less-than plt))
                                (~? (~@ #:result/stx result))]))
  )

(define-syntax (define-bnf/quick stx)
  (syntax-parse stx
    [(_ name:id
        (~or (~optional (~seq #:layout-parsers layout-parsers:expr))
             (~optional (~seq #:layout layout-arg:expr))
             ;(~optional (~seq #:result/stx result/stx:expr))
             ;(~optional (~seq #:result/bare result/bare:expr))
             ;(~optional (~seq #:main-arm main-arm-arg:id))
             )
        ...
        [(~or (~optional (~and (~datum /) ignore-arm-name))
              ;; TODO - I don't like how I have % implemented -- layout in the underlying macro is ternary -- required, optional, or none.  This only allows setting an arm to none.
              (~optional (~and (~datum %) no-layout-for-arm)))
         ...
         (~and arm-name:id unspecial:unspecial-expr)
         arm-spec:bnf-arm-alt-spec/quick ...]
        ...)
     #'(define name
         (let ()
           (define-bnf name
             (~? (~@ #:layout-parsers layout-parsers))
             (~? (~@ #:layout layout-arg))
             [arm-name
              (~? (~@ #:ignore-arm-name? 'ignore-arm-name))
              (~? (~@ #:layout (and 'no-layout-for-arm 'none)))
              arm-spec.nonquick ...] ...)
           name))]))

(module+ test

  (define-bnf/quick quick-simple
    [stmt [expr]
          ["FOR" "foo" "in" expr "do" stmt]]
    [expr [(as-syntax "a")]
          [(as-syntax "b")]
          [/ "(" @ expr * / ")" :: (λ args (cons 'LIST args))]
          [expr "badmirror" expr]
          ["q" @ "1" *]
          ["p" @ "2" ?]
          ["r" @ #(/ "3" expr +) ?]
          [@ (|| "#1" "#2" #("one" "two")) + "#3"]
          [first = expr "mirror" (result-filter
                                  expr
                                  (λ (r)
                                    (equal? (syntax->datum r)
                                            (syntax->datum
                                             (parse-derivation-result first))))) ]])

  (check se/datum?
         (wp*/r "a" quick-simple)
         (list #'(stmt (expr "a"))))
  (check se/datum?
         (wp*/r "q1" quick-simple)
         (list #'(stmt (expr "q" "1"))))
  (check se/datum?
         (wp*/r "q" quick-simple)
         (list #'(stmt (expr "q"))))
  (check se/datum?
         (wp*/r "p2" quick-simple)
         (list #'(stmt (expr "p" "2"))))
  (check se/datum?
         (wp*/r "p" quick-simple)
         (list #'(stmt (expr "p"))))
  (check se/datum?
         (wp*/r "r" quick-simple)
         (list #'(stmt (expr "r"))))
  (fail "there are 3 commented-out failing tests here, I just want a smaller fail footprint while I work on other things.")
  #;(check se/datum?
         (wp*/r "r3(p)" quick-simple)
         (list #'(stmt (expr "r" (expr (LIST (expr "p")))))))
  #;(check se/datum?
         (wp*/r "r3p" quick-simple)
         (list #'(stmt (expr "r" (expr "p")))))
  #;(check se/datum?
         (wp*/r "r3p2pp2" quick-simple)
         (list #'(stmt (expr "r" (expr "p" "2") (expr "p") (expr "p" "2")))))
  (check se/datum?
         (wp*/r "(aa(ba)ba)" quick-simple)
         (list #'(stmt (LIST (expr "a")
                             (expr "a")
                             (LIST (expr "b") (expr "a"))
                             (expr "b")
                             (expr "a")))))
  (check se/datum?
         (wp*/r "a mirror a" quick-simple)
         (list #'(stmt (expr (expr "a") "mirror" (expr "a")))))
  (check se/datum?
         (wp*/r "a mirror b" quick-simple)
         '())
  (check se/datum?
         (wp*/r "#1#2#1 #1#3" quick-simple)
         (list #'(stmt (expr "#1" "#2" "#1" "#1" "#3"))))
  (check se/datum?
         (wp*/r "#1 one two#1 #3" quick-simple)
         (list #'(stmt (expr "#1" ("one" "two") "#1" "#3"))))
  (check se/datum?
         (wp*/r "FOR foo in q1111 do FOR foo in #1#1#3 do (a b)" quick-simple)
         (list #'(stmt "FOR" "foo" "in" (expr "q" "1" "1" "1" "1") "do"
                       (stmt "FOR" "foo" "in" (expr "#1" "#1" "#3") "do"
                             (stmt (LIST (expr "a") (expr "b")))))))

  (define-bnf/quick bnf-test-1/quick
    [statement ["pass"]
               ["for" id "in" expression "do" statement]
               ["{" @ statement * "}"]
               [expression]]
    [expression [number-parser]
                [id]
                [expression "+" expression & 'left]
                [expression "*" expression
                            & 'right
                            > "+"]]
    [id ["x"]])

  (check se/datum?
         (wp*/r "pass" bnf-test-1/quick)
         (list #'(statement "pass")))

  (check se/datum?
         (wp*/r "53" bnf-test-1/quick)
         (list #'(statement (expression 53))))
  (check se/datum?
         (wp*/r "53 + 27" bnf-test-1/quick)
         (list #'(statement (expression (expression 53) "+" (expression 27)))))

  (check se/datum?
         (wp*/r "5 + 67 * 3" bnf-test-1/quick)
         (list #'(statement (expression (expression 5)
                                        "+"
                                        (expression (expression 67)
                                                    "*"
                                                    (expression 3))))))


  (check se/datum?
         (wp*/r "for x in 27 + 13 do { 555 * x }" bnf-test-1/quick)
         (list #'(statement "for" (id "x") "in"
                            (expression (expression 27)
                                        "+"
                                        (expression 13))
                            "do"
                            (statement "{" (statement
                                            (expression (expression 555)
                                                        "*"
                                                        (expression (id "x"))))
                                       "}"))))
  (check se/datum?
         (wp*/r "{ 5 6 7 }" bnf-test-1/quick)
         (list #'(statement "{"
                            (statement (expression 5))
                            (statement (expression 6))
                            (statement (expression 7))
                            "}")))

  (define-bnf/quick with-subseq
    [an-arm ["a" @@ #("b" "c") + "d"]])

  (check se/datum?
         (wp*/r "a b c b c b c d" with-subseq)
         (list #'(an-arm "a" "b" "c" "b" "c" "b" "c" "d")))

  )

