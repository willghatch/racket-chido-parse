#lang racket/base

(provide
 define-bnf-arm
 define-bnf
 define-bnf/quick

 readtable-extend-as-bnf-arm
 extend-bnf
 )

(require
 "core.rkt"
 "readtable-parser.rkt"
 "procedural-combinators.rkt"
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
   "test-util.rkt"
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
              ...]
             #:attr parser #f))
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
         (define layout-arg-use (~? layout-arg bnf-default-layout-requirement))
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
            (set-chido-readtable-symbol-support empty-chido-readtable #f)
            bnf-layout-mode-key
            layout-arg-use))

         (define rt2
           (for/fold ([rt rt1])
                     ([parser (~? layout-parsers bnf-default-layout-parsers)])
             (extend-chido-readtable rt 'layout parser)))

         (define rt3 (readtable-extend-as-bnf-arm rt2
                                                  #:result/stx use-result/stx?
                                                  #:result/bare use-result/bare?
                                                  #:arm-name arm-name
                                                  spec ...))
         (define arm-name rt3))]))


(define-syntax (readtable-extend-as-bnf-arm stx)
  (syntax-parse stx
    [(_ arm:expr
        (~or (~optional (~seq #:arm-name arm-name:id))
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
                             #f
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
                [alts (list (~? spec.parser
                                (let ([send-result/stx
                                       (or result/stx (and (not result/bare)
                                                           use-result/stx?))]
                                      [send-result/bare
                                       (or result/bare (and (not result/stx)
                                                            use-result/bare?))])
                                  (binding-sequence
                                   spec.elem ...
                                   #:result/stx send-result/stx
                                   #:result/bare send-result/bare
                                   #:between bnf-inserted-layout-parser
                                   #:name (or alt-name/direct
                                              alt-name/inferred))))
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
                    rt extension-type parser
                    #:operator op-type
                    #:associativity a
                    #:precidence-greater-than pgt
                    #:precidence-less-than plt))]
                [rt/blacklisted (chido-readtable-blacklist-symbols
                                 rt/extended
                                 (flatten (list (list 'symbol-blacklist ...)
                                                ...)))])
           rt/blacklisted))]))






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
    #:result/bare #t
    #:layout 'none
    ["a" [: #:ignore #t "b"] "c"]
    ["a" "a" "b"])
  (define-bnf-arm test1/layout
    #:result/bare #t
    #:layout 'required
    ["a" [: #:ignore #t "b"] "c" #:name "test-name-1"]
    ["a" "a" "b"])
  (check-equal? (->results (parse* (open-input-string "abc")
                                   (chido-readtable->read1 test1)))
                '(("a" "c")))
  (check-equal? (->results (parse* (open-input-string "aab")
                                   (chido-readtable->read1 test1)))
                '(("a" "a" "b")))
  (check-equal? (->results (parse* (open-input-string "abbc")
                                   (chido-readtable->read1 test1)))
                '())

  (check-equal? (->results (parse* (open-input-string "a b c")
                                   (chido-readtable->read1 test1/layout)))
                '(("a" "c")))
  (check-equal? (->results (parse* (open-input-string "a a b")
                                   (chido-readtable->read1 test1/layout)))
                '(("a" "a" "b")))
  (check-equal? (->results (parse* (open-input-string "a b b c")
                                   (chido-readtable->read1 test1/layout)))
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
    ["n"]
    [test2 "+" test2 #:associativity 'left]
    [test2 "*" test2 #:associativity 'left #:precidence-greater-than "+"]
    [test2 "^" test2 #:associativity 'right #:precidence-greater-than "*"]
    [test2 "++" #:precidence-less-than '("*") #:precidence-greater-than "+"]
    ["if" test2 "then" test2 "else" test2 #:precidence-less-than "+"])

  (check-equal? (->results (parse* (open-input-string "n")
                                   (chido-readtable->read1 test2)))
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
  (λ (self)
    (proc-parser
     (λ (port)
       (chido-parse-parameterize
        ([current-bnf self])
        (parse* port
                (hash-ref (bnf-parser-arm-hash self)
                          (bnf-parser-main-arm-key self))))))))

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
             (~optional (~seq #:result/stx result/stx:expr))
             (~optional (~seq #:result/bare result/bare:expr))
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
           (define result/stx/inherit (~? result/stx #f))
           (define result/bare/inherit (~? result/bare #f))
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
                                     (current-bnf))))))

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
           (define inner-bnf-name
             (let ([arm-name (let ()
                               (define-bnf-arm arm-name
                                 layout-parsers-inherit (list master-layout-parser)
                                 layout-inherit layout/inherit
                                 result/bare-inherit result/bare/inherit
                                 result/stx-inherit result/stx/inherit
                                 arm-spec/kw ...)
                               arm-name)]
                   ...)
               (bnf-parser 'main-arm
                           (hash (~@ 'arm-name arm-name) ...)
                           layout-parsers/use
                           layout-alt)))
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

  (check-equal? (->results (whole-parse*
                            (open-input-string "pass")
                            bnf-test-1))
                '("pass"))
  (check-equal? (->results (whole-parse*
                            (open-input-string "5 + 67 * 3")
                            bnf-test-1))
                '((5 "+" (67 "*" 3))))
  (check-equal? (->results (whole-parse*
                            (open-input-string "for x in 27 + 13 do { 555 * x }")
                            bnf-test-1))
                '(("for" "x" "in" (27 "+" 13) "do" ("{" (555 "*" "x") "}"))))
  (check-equal? (->results (whole-parse*
                            (open-input-string "{ 5 6 7 }")
                            bnf-test-1))
                '(("{" 5 6 7 "}")))

  (define bnf-test-2
    (extend-bnf
     bnf-test-1
     #:result/bare #t
     [statement ["break"]
                [id ":=" expression]]
     [expression ["let" id "=" expression "in" expression
                        #:precidence-greater-than '("*")]]
     [id]))

  (check-equal? (->results (whole-parse*
                            (open-input-string "{ pass break 2 + let x = 5 in 2 + 2 }")
                            bnf-test-2))
                '(("{" "pass" "break" ((2 "+" ("let" "x" "=" 5 "in" 2)) "+" 2) "}")))

  )



(begin-for-syntax
  (define-syntax-class ats
    (pattern x:id
             #:when (regexp-match "@+" (symbol->string (syntax->datum #'x)))
             #:attr number (datum->syntax #'here
                                          (string-length
                                           (symbol->string (syntax->datum #'x))))))
  (define-splicing-syntax-class binding-sequence-elem/quick
    (pattern (~seq
              (~or (~optional (~seq name:id (~datum =)))
                   (~optional (~and ignore-given (~datum /)))
                   (~optional splice-given:ats))
              ...
              (~and (~not (~or (~datum =)
                               (~datum /)
                               (~datum *)
                               (~datum +)
                               (~datum &)
                               (~datum <)
                               (~datum >)
                               at-seq:ats))
                    parser-given:expr)
              (~or (~optional (~and star (~datum *)))
                   (~optional (~and plus (~datum +)))
                   ))
             #:attr ignore (if (attribute ignore-given)
                               #'#t
                               #'#f)
             #:attr nonquick #'[:
                                parser-given
                                (~? (~@ #:bind name))
                                #:ignore ignore
                                #:splice (~? splice-given.number #f)
                                #:repeat-min (cond [(~? star #f) 0]
                                                   [(~? plus #f) 1]
                                                   [else #f])]))
  (define-syntax-class bnf-arm-alt-spec/quick
    (pattern [elem:binding-sequence-elem/quick
              ...+
              (~or (~optional (~seq (~datum &) assoc:expr))
                   (~optional (~seq (~datum >) pgt:expr))
                   (~optional (~seq (~datum <) plt:expr))
                   )
              ...]
             #:attr nonquick #'[elem.nonquick
                                ...
                                (~? (~@ #:associativity assoc))
                                (~? (~@ #:precidence-greater-than pgt))
                                (~? (~@ #:precidence-less-than plt))]))
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
        [arm-name:id arm-spec:bnf-arm-alt-spec/quick ...]
        ...)
     #'(define-bnf name
         (~? (~@ #:layout-parsers layout-parsers))
         (~? (~@ #:layout layout-arg))
         [arm-name arm-spec.nonquick ...] ...)]))

(module+ test

  (define-bnf/quick quick-simple
    [thing [(as-syntax "a")]
           [(as-syntax "b")]
           [/ "(" @ thing * / ")"]
           [thing "badmirror" thing]
           [first = thing "mirror" (parse-filter
                                    thing
                                    (λ (port derivation)
                                      (equal? (syntax->datum
                                               (parse-derivation-result derivation))
                                              (syntax->datum
                                               (parse-derivation-result first))))) ]])

  (check se/datum?
         (->results (whole-parse* (open-input-string "a")
                                  quick-simple))
         (list #'"a"))
  (check se/datum?
         (->results (whole-parse* (open-input-string "b")
                                  quick-simple))
         (list #'"b"))
  (check se/datum?
         (->results (whole-parse* (open-input-string "(aa(ba)ba)")
                                  quick-simple))
         (list #'("a" "a" ("b" "a") "b" "a")))
  (check se/datum?
         (->results (whole-parse* (open-input-string "a mirror a")
                                  quick-simple))
         (list #'("a" "mirror" "a")))
  (check se/datum?
         (->results (whole-parse* (open-input-string "a mirror b")
                                  quick-simple))
         '())

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
         (->results (whole-parse* (open-input-string "pass")
                     bnf-test-1/quick))
         (list #'"pass"))

  (check se/datum?
         (->results (whole-parse*
                     (open-input-string "53")
                     bnf-test-1/quick))
         (list #'53))
  (check se/datum?
         (->results (whole-parse*
                     (open-input-string "53 + 27")
                     bnf-test-1/quick))
         (list #'(53 "+" 27)))

  (check se/datum?
         (->results (whole-parse*
                     (open-input-string "5 + 67 * 3")
                     bnf-test-1/quick))
         (list #'(5 "+" (67 "*" 3))))


  (check se/datum?
         (->results (whole-parse*
                     (open-input-string "for x in 27 + 13 do { 555 * x }")
                     bnf-test-1/quick))
         (list #'("for" "x" "in" (27 "+" 13) "do" ("{" (555 "*" "x") "}"))))
  (check se/datum?
         (->results (whole-parse*
                     (open-input-string "{ 5 6 7 }")
                     bnf-test-1/quick))
         (list #'("{" 5 6 7 "}")))

  )
