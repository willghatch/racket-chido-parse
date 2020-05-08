#lang racket/base

#|
Arguably the point of Racket's readtable is to have an extensible alternate parser that has a built-in symbol parser that is effected by each added alternate.
This is an implementation of the same idea, but also adding support for operators with declarative precedence and associativity and potentially other extra features.
|#

(require racket/contract/base)
(define symbol-effect/c
  (or/c 'terminating 'soft-terminating 'nonterminating
        'terminating-layout 'soft-terminating-layout 'nonterminating-layout
        'left-recursive-nonterminating))
(provide
 ;; TODO - the names provided should maybe be chido-readtable-* ...
 (contract-out
  ;; TODO - re-think what the empty readtable should be...
  [empty-chido-readtable chido-readtable?]
  [extend-chido-readtable
   (->* (symbol-effect/c
         ;; parser...
         any/c
         chido-readtable?)
        (#:operator (or/c #f 'infix 'prefix 'postfix)
         #:precedence-less-than (or/c any/c (listof any/c))
         #:precedence-greater-than (or/c any/c (listof any/c))
         #:associativity (or/c 'left 'right #f)
         ;#:symbol-blacklist (listof (or/c symbol? string?))
         )
        chido-readtable?)]
  [chido-readtable-add-list-parser
   (->* (string? string? chido-readtable?)
        (#:wrapper (or/c #f symbol? (-> syntax? syntax?))
         #:inside-readtable (or/c #f chido-readtable? (-> chido-readtable?))
         #:readtable-symbol-effect symbol-effect/c)
        chido-readtable?)]
  [chido-readtable-add-raw-string-parser
   (->* (string? string? chido-readtable?)
        (#:wrapper (or/c #f symbol? (-> syntax? syntax?))
         #:readtable-symbol-effect symbol-effect/c)
        chido-readtable?)]
  [chido-readtable->read1 (-> chido-readtable? any/c)]
  [chido-readtable->read1/layout (-> chido-readtable? any/c)]
  [chido-readtable->read* (-> chido-readtable? any/c)]
  [chido-readtable->read+ (-> chido-readtable? any/c)]
  [chido-readtable->layout1 (-> chido-readtable? any/c)]
  [chido-readtable->layout* (-> chido-readtable? any/c)]
  [chido-readtable->layout+ (-> chido-readtable? any/c)]
  [set-chido-readtable-symbol-support (-> chido-readtable? any/c chido-readtable?)]
  [set-chido-readtable-name (-> chido-readtable? any/c chido-readtable?)]
  [chido-readtable-symbol-support? (-> chido-readtable? boolean?)]
  [chido-readtable-blacklist-symbols (-> (listof (or/c symbol? string?))
                                         chido-readtable?
                                         chido-readtable?)]
  [chido-readtable-dict-ref (->* (chido-readtable? any/c) (any/c) any/c)]
  [chido-readtable-dict-set (-> chido-readtable? any/c any/c chido-readtable?)]
  [chido-readtable-add-mixfix-operator
   (->* ((or/c symbol? string?) chido-readtable?)
        (#:layout (or/c 'required 'optional 'none)
         #:precedence-greater-than (or/c any/c (listof any/c))
         #:precedence-less-than (or/c any/c (listof any/c))
         #:associativity (or/c 'left 'right #f))
        any/c)]
  )
 (rename-out [symbol-effect/c chido-readtable-symbol-effect/c])
 chido-readtable?
 chido-readtable-name
 extend-chido-readtable*
 chido-readtable-add-mixfix-operators
 current-chido-readtable

 current-chido-readtable-read1-parser
 current-chido-readtable-layout*-parser
 current-chido-readtable-layout+-parser
 current-chido-readtable-symbol-parser

 ;; TODO - maybe not these?
 hash-t-parser
 hash-f-parser
 racket-style-string-parser

 )

(require
 "core-use.rkt"
 "procedural-combinators.rkt"
 "trie.rkt"
 "parameters.rkt"
 "parse-stream.rkt"
 racket/match
 racket/string
 racket/list
 racket/dict
 racket/set
 (for-syntax
  racket/base
  syntax/parse
  ))

(struct chido-readtable
  (
   name
   ;; Core parsers
   terminating-parsers
   soft-terminating-parsers
   nonterminating-parsers

   left-recursive-nonterminating-parsers

   terminating-layout-parsers
   soft-terminating-layout-parsers
   nonterminating-layout-parsers

   ;;; Options

   ;; If symbol support is off, then terminating, soft-terminating, and nonterminating parsers are indistinguishable.  The difference is only how it effects the built-in symbol parser.
   ;; For readtable-style extension, symbols need to be built-in so adding parsers effects the symbol parser implicitly.
   ;; TODO - I should add more ways to effect the built-in symbol parser.  Eg. maybe soft-terminating-failure and nonterminating-failure parsers, which delimit symbols a la soft-terminating and nonterminating, making the symbol parser fail or not run when they succeed, but that don't create a derivation.  IE they make the symbol parser fail with a message saying that the other parser succeeded.  Basically programatic filters for symbols beyond just having a blacklist.  Eg. this would allow a convention for operator names (eg. they must be in <> like <+>, or they must be in : like :+: or :where:, etc, then fail symbols that match that pattern.)
   ;; TODO - operator precedence programability -- if you have an operator family, such as an operator that reads the operator string as some family of symbols like :+:, :where:, etc, does it make sense to have some more complicated and programmable way of doing precedence and associativity?
   symbol-support?
   symbol-result-transformer
   literal-left-delimiter
   literal-right-delimiter
   symbol-escape ;; IE backslash
   number-support?
   complex-number-support?

   ;; symbol blacklist, mostly so operators can not be symbols.
   symbol-blacklist

   ;;; operator info

   ;; TODO - add numeric precedence in addition to relative precedence.  Then users can choose one way or the other.  If both are used, make them separate -- IE compare precedence based on lattice and based on number, don't make the number operator transitive with the lattice operator.  Maybe also error if there is a relationship both ways?  Or maybe error when a readtable has both numeric and relative precedence?
   ;; TODO - the current design makes all precedence relationships transitive.  Maybe both transitive and intransitive relationships should be allowed?
   ;; hash from parser to set of parsers that are immediately above the key parser in the precedence lattice
   precedence-immediate-greater-relations
   ;; hash from parser to set of parsers that are immediately below the key parser in the precedence lattice
   precedence-immediate-lesser-relations

   ;; lists of parsers that are marked as these things
   prefix-operators
   postfix-operators
   ;; dict of binary operator to its associativity
   infix-operator->associativity
   ;; dict of operator names to the parser objects
   operator-name->operator


   ;; Extra storage for miscellaneous applications, maybe user-defined.
   dict


   ;; Basically these are cached results
   [flush-state? #:mutable]
   [terminating-trie #:mutable]
   [soft-terminating-trie #:mutable]
   [nonterminating-trie #:mutable]
   [terminating-layout-trie #:mutable]
   [soft-terminating-layout-trie #:mutable]
   [nonterminating-layout-trie #:mutable]

   ;; These exist to ensure parsers created with chido-readtable are always eq?
   [symbol-parser #:mutable]
   [layout1-parser #:mutable]
   [layout*-parser #:mutable]
   [layout+-parser #:mutable]
   [read1-parser #:mutable]
   [layout*+read1-parser #:mutable]
   [read*-parser #:mutable]
   [read+-parser #:mutable]

   ;; To cache precedence lattice traversal and cycle detection
   [precedence-transitive-greater-relations #:mutable]
   )
  #:property prop:custom-parser
  (λ (self) (chido-readtable->read1 self))
  #:methods gen:dict
  [(define (dict-ref rt key
                     [default (λ () (error 'chido-readtable-dict-ref
                                           "Key not found: ~v" key))])
     (hash-ref (chido-readtable-dict rt) key default))
   (define (dict-set rt key val)
     (struct-copy chido-readtable rt
                  [dict (hash-set (chido-readtable-dict rt) key val)]))]
  )


(define empty-chido-readtable
  (chido-readtable
   ;; name
   #f
   ;; core parsers
   '() '() '() '() '() '() '()

   ;;; options

   ;; symbol-support?
   #t
   ;; symbol-result-transformer
   #f
   ;; TODO - also, when literal delimiters are NOT equal, should they be nestable?  I think yes.  Maybe there should be an option?
   ;; literal delimiters
   #f #f
   ;; symbol escape
   #f
   ;; number support
   #f
   ;; complex number support
   #f
   ;; symbol blacklist
   '()

   ;;; operator stuff

   ;; precedence-immediate-greater-relations
   (hash)
   ;; precedence-immediate-lesser-relations
   (hash)
   ;; prefix-operators
   '()
   ;; postfix-operators
   '()
   ;; infix-operator->associativity
   (hash)
   ;; operator-name->operator
   (hash)

   ;; dict
   (hash)

   ;;; cached stuff

   ;; flush-state?
   #t
   ;; tries
   empty-trie
   empty-trie
   empty-trie
   empty-trie
   empty-trie
   empty-trie
   ;; parsers
   #f #f #f #f #f #f #f #f
   ;; precedence-transitive-greater-relations
   (hash)
   ))

(define (chido-readtable-dict-ref rt key
                                  [default (λ () (error 'chido-readtable-dict-ref
                                                        "Key not found: ~v" key))])
  (dict-ref rt key default))
(define (chido-readtable-dict-set rt key val)
  (dict-set rt key val))

(define (extend-chido-readtable symbol-effect parser rt
                                ;; TODO - better name for symbol-effect, and make it a keyword argument.
                                #:operator [operator #f]
                                #:precedence-less-than [precedence-less-than '()]
                                #:precedence-greater-than [precedence-greater-than '()]
                                #:associativity [associativity #f]
                                #:symbol-blacklist [symbol-blacklist #f]
                                )
  ;; symbol-effect is 'terminating, 'soft-terminating, 'nonterminating, 'left-recursive-nonterminating,  'terminating-layout, 'soft-terminating-layout, or 'nonterminating-layout
  ;; operator is #f, 'infix, 'prefix, or 'postfix
  ;; associativity is #f, 'left, or 'right
  ;; precedence lists are for names of other operators that are immediately greater or lesser in the precedence lattice.
  ;; symbol-blacklist can be #f to do nothing, #t to add the parser name to the symbol blacklist (for the common case that the operator name is the parser name), or a list of symbols or strings to blacklist.
  ;; TODO - maybe symbol-blacklist doesn't belong here, but its primary motivation is to blacklist operator names...

  #| TODO - make symbol-effect a keyword argument, make the default nonterminating? |#
  (when (and (member symbol-effect '(terminating-layout
                                     soft-terminating-layout
                                     nonterminating-layout))
             operator)
    (error 'extend-chido-readtable
           "can't add operator parsers to layout parsers"))
  (when (and (member operator '(infix postfix))
             (not (eq? symbol-effect 'left-recursive-nonterminating)))
    (error 'extend-chido-readtable
           "infix and postfix operators must be added as left-recursive-nonterminating parsers"))

  (define pre-blacklist
    (match symbol-effect
      ['terminating
       (struct-copy
        chido-readtable
        rt
        [terminating-parsers
         (cons parser (chido-readtable-terminating-parsers rt))]
        [flush-state? #t])]
      ['soft-terminating
       (struct-copy
        chido-readtable
        rt
        [soft-terminating-parsers
         (cons parser (chido-readtable-soft-terminating-parsers rt))]
        [flush-state? #t])]
      ['nonterminating
       (struct-copy
        chido-readtable
        rt
        [nonterminating-parsers
         (cons parser (chido-readtable-nonterminating-parsers rt))]
        [flush-state? #t])]
      ['left-recursive-nonterminating
       (struct-copy
        chido-readtable
        rt
        [left-recursive-nonterminating-parsers
         (cons parser (chido-readtable-left-recursive-nonterminating-parsers rt))]
        [flush-state? #t])]
      ['terminating-layout
       (struct-copy
        chido-readtable
        rt
        [terminating-layout-parsers
         (cons parser (chido-readtable-terminating-layout-parsers rt))]
        [flush-state? #t])]
      ['soft-terminating-layout
       (struct-copy
        chido-readtable
        rt
        [soft-terminating-layout-parsers
         (cons parser (chido-readtable-soft-terminating-layout-parsers rt))]
        [flush-state? #t])]
      ['nonterminating-layout
       (struct-copy
        chido-readtable
        rt
        [nonterminating-layout-parsers
         (cons parser (chido-readtable-nonterminating-layout-parsers rt))]
        [flush-state? #t])]))
  (define pre-op
    (match symbol-blacklist
      [#f pre-blacklist]
      [#t (chido-readtable-blacklist-symbols
           (list (->symbol (parser-name parser)))
           pre-blacklist)]
      [(list strsym ...)
       (chido-readtable-blacklist-symbols (map ->symbol strsym) pre-blacklist)]))

  (define op-name->op
    (chido-readtable-operator-name->operator pre-op))
  (define (op-resolve op-or-name)
    (define err (λ () (error 'extend-chido-readtable
                             "bad operator name: ~v\n" op-or-name)))
    (cond [(symbol? op-or-name) (dict-ref op-name->op (symbol->string op-or-name)
                                          err)]
          [(string? op-or-name) (dict-ref op-name->op op-or-name err)]
          [(parser? op-or-name) op-or-name]
          [else (err)]))

  (define (precedence->list prec)
    (cond [(not prec) '()]
          [(list? prec) prec]
          [else (list prec)]))

  (if operator
      (struct-copy
       chido-readtable
       (match operator
         ['infix (struct-copy chido-readtable
                              pre-op
                              [infix-operator->associativity
                               (dict-set (chido-readtable-infix-operator->associativity rt)
                                         parser
                                         associativity)])]
         ['prefix (struct-copy chido-readtable
                               pre-op
                               [prefix-operators
                                (cons parser
                                      (chido-readtable-prefix-operators rt))])]
         ['postfix (struct-copy chido-readtable
                                pre-op
                                [postfix-operators
                                 (cons parser
                                       (chido-readtable-postfix-operators rt))])])
       [precedence-immediate-greater-relations
        (dict-set
         (chido-readtable-precedence-immediate-greater-relations rt)
         parser
         (map op-resolve (precedence->list precedence-less-than)))]
       [precedence-immediate-lesser-relations
        (dict-set
         (chido-readtable-precedence-immediate-lesser-relations rt)
         parser
         (map op-resolve (precedence->list precedence-greater-than)))]
       [operator-name->operator
        (dict-set op-name->op (parser-name parser) parser)])
      pre-op))

(define (extend-chido-readtable* rt . args)
  (match args
    [(list type parser rest-args ...)
     (apply extend-chido-readtable*
            (extend-chido-readtable type parser rt)
            rest-args)]
    [(list) rt]
    [else (error 'extend-chido-readtable* "bad number of arguments")]))

(define (set-chido-readtable-name rt name)
  (struct-copy chido-readtable rt
               [name name]
               [flush-state? #t]))

(define (set-chido-readtable-symbol-support rt bool)
  (struct-copy chido-readtable rt
               [symbol-support? (not (not bool))]
               [flush-state? #t]))

(define (chido-readtable-blacklist-symbols symbols rt)
  (struct-copy chido-readtable rt
               [symbol-blacklist (remove-duplicates
                                  (append (map ->symbol symbols)
                                          (chido-readtable-symbol-blacklist rt)))]))

(define (parser-list->trie parsers)
  (for/fold ([t empty-trie])
            ([p parsers])
    (trie-add t (parser-prefix p) p)))

(define (chido-readtable-populate-cache! rt)
  (when (chido-readtable-flush-state? rt)
    (define (operator-wrap parser)
      (cond [(infix-operator? rt parser)
             (parse-filter parser (filter-infix-operator-derivation rt))]
            [(prefix-operator? rt parser)
             (parse-filter parser (filter-prefix-operator-derivation rt))]
            [(postfix-operator? rt parser)
             (parse-filter parser (filter-postfix-operator-derivation rt))]
            [else parser]))
    (define terminating/wrap
      (map operator-wrap (chido-readtable-terminating-parsers rt)))
    (define soft-terminating/wrap
      (map operator-wrap (chido-readtable-soft-terminating-parsers rt)))
    (define nonterminating/wrap
      (map operator-wrap (chido-readtable-nonterminating-parsers rt)))
    (define left-recursive-nonterminating/wrap
      (map operator-wrap
           (chido-readtable-left-recursive-nonterminating-parsers rt)))

    (define name-format
      (let ([name (chido-readtable-name rt)])
        (if name
            (format "-~a-" name)
            "-")))

    (set-chido-readtable-terminating-trie!
     rt
     (parser-list->trie terminating/wrap))
    (set-chido-readtable-soft-terminating-trie!
     rt
     (parser-list->trie soft-terminating/wrap))
    (set-chido-readtable-nonterminating-trie!
     rt
     (parser-list->trie nonterminating/wrap))
    (set-chido-readtable-terminating-layout-trie!
     rt
     (parser-list->trie (chido-readtable-terminating-layout-parsers rt)))
    (set-chido-readtable-soft-terminating-layout-trie!
     rt
     (parser-list->trie (chido-readtable-soft-terminating-layout-parsers rt)))
    (set-chido-readtable-nonterminating-layout-trie!
     rt
     (parser-list->trie (chido-readtable-nonterminating-layout-parsers rt)))
    (set-chido-readtable-symbol-parser! rt (symbol/number-parser rt))
    (set-chido-readtable-layout1-parser!
     rt (make-alt-parser (format "chido-readtable~alayout1" name-format)
                         (append
                          (chido-readtable-terminating-layout-parsers rt)
                          (chido-readtable-soft-terminating-layout-parsers rt)
                          (chido-readtable-nonterminating-layout-parsers rt))))
    (set-chido-readtable-layout*-parser!
     rt (kleene-star (chido-readtable-layout1-parser rt)
                     #:derive (λ (elems) (make-parse-derivation
                                          '() #:derivations elems))
                     #:greedy? #t))
    (set-chido-readtable-layout+-parser!
     rt (kleene-plus (chido-readtable-layout1-parser rt)
                     #:derive (λ (elems) (make-parse-derivation
                                          '() #:derivations elems))
                     #:greedy? #t))
    (set-chido-readtable-read1-parser!
     rt
     ;; TODO - better name!
     (proc-parser
      #:name (format "chido-readtable~aread1" name-format)
      (let* ([alt (make-alt-parser (format "chido-readtable~aread1/base-alt"
                                           name-format)
                                   (append
                                    nonterminating/wrap
                                    soft-terminating/wrap
                                    terminating/wrap))]
             [left-recursive-nonterminating-alt
              (make-alt-parser (format "chido-readtable~aread1/lrn-alt" name-format)
                               left-recursive-nonterminating/wrap)])
        (λ (port)
          (chido-parse-parameterize
           ([current-chido-readtable rt])
           (define parsers-result (parse* port alt))
           (define symbol-result
             (cond
               [(not (chido-readtable-symbol-support? rt)) parsers-result]
               [(parse-failure? parsers-result)
                (let ([symbol-result
                       (parse* port (chido-readtable-symbol-parser rt))])
                  (parse-stream-cons parsers-result symbol-result))]
               [else parsers-result]))
           (parse-stream-cons
                symbol-result
                (parse* port left-recursive-nonterminating-alt)))))
      #:use-port? #f
      #:promise-no-left-recursion?
      (not (ormap parser-potentially-left-recursive?
                  (append nonterminating/wrap
                          soft-terminating/wrap
                          terminating/wrap
                          left-recursive-nonterminating/wrap
                          (chido-readtable-terminating-layout-parsers rt)
                          (chido-readtable-soft-terminating-layout-parsers rt)
                          (chido-readtable-nonterminating-layout-parsers rt)
                          )))))
    (set-chido-readtable-layout*+read1-parser!
     rt
     (sequence
      (chido-readtable-layout*-parser rt)
      (chido-readtable-read1-parser rt)
      #:derive (λ derivations
                 (make-parse-derivation
                  (λ (src line col pos span derivations)
                    (parse-derivation-result (second derivations)))
                  #:derivations derivations))))

    (let ([with-content-parser
            (sequence
             #:name (format "chido-readtable~aread+" name-format)
             (kleene-plus (chido-readtable-layout*+read1-parser rt) #:greedy? #t)
             (chido-readtable-layout*-parser rt)
             #:derive (λ derivations
                        (make-parse-derivation
                         (λ (src line col pos span derivations)
                           (parse-derivation-result (first derivations)))
                         #:derivations derivations)))]
          [no-content-parser (chido-readtable-layout*-parser rt)])
      (set-chido-readtable-read+-parser! rt with-content-parser)
      (set-chido-readtable-read*-parser!
       rt
       (make-alt-parser (format "chido-readtable~aread*" name-format)
                        (list with-content-parser no-content-parser))))

    ;;; Set transitive operator precedence hash

    (define all-direct-greaters
      ;; Because precedence can be declared by a greater than or less than relation,
      ;; I need to reverse one side to get all one relation.
      (for/fold ([ghash (chido-readtable-precedence-immediate-greater-relations rt)])
                ([op (append (chido-readtable-prefix-operators rt)
                             (chido-readtable-postfix-operators rt)
                             (dict-keys
                              (chido-readtable-infix-operator->associativity rt)))])
        (for/fold ([ghash ghash])
                  ([lesser-op
                    (dict-ref
                     (chido-readtable-precedence-immediate-lesser-relations rt)
                     op)])
          (dict-set ghash lesser-op (cons op (dict-ref ghash lesser-op '()))))))
    (define (compute-transitive-greaters orig-op work-set done-set greater-set)
      (if (set-empty? work-set)
          (if (set-member? greater-set orig-op)
              (error 'chido-readtable
                     "Circular operator precedence detected for: ~a"
                     (parser-name orig-op))
              greater-set)
          (let* ([new-greaters
                  (dict-ref all-direct-greaters (set-first work-set) '())]
                 [new-done-set (set-add done-set (set-first work-set))]
                 [new-work-set (set-rest work-set)]
                 [new-work-set (set-union new-work-set
                                          (set-subtract new-greaters new-done-set))]
                 [new-greater-set (set-union new-greaters greater-set)])
            (compute-transitive-greaters orig-op
                                         new-work-set
                                         new-done-set
                                         new-greater-set))))
    (set-chido-readtable-precedence-transitive-greater-relations!
     rt
     (for/hash ([op (dict-keys all-direct-greaters)])
       (values op (compute-transitive-greaters op (list op) '() '()))))

    ;;; The readtable is now ready to use...
    (set-chido-readtable-flush-state?! rt #f)))


(define ((parse-symbol/number-func rt) pb)
  ;; TODO - handle symbol escapes and literal delimiters

  (define start-pos (port-broker-start-position pb))
  ;; Trie-pairs are (cons trie n),
  ;; where n is the prefix length of trie that has matched so far.
  (define (rec/main len hard-trie-pairs soft-trie-pairs)
    ;; The trie pairs always come in sorted by prefix length matched, small to large.
    (define new-hard-tries (list*
                            (cons (chido-readtable-terminating-trie rt) 0)
                            (cons (chido-readtable-terminating-layout-trie rt) 0)
                            hard-trie-pairs))
    (define hard-delimited-lengths
      (filter-map (λ (tp) (and (not (trie-bare? (car tp)))
                               (cdr tp)))
                  new-hard-tries))
    ;; If any hard-terminating parsers prefixes are reached, we want to use
    ;; the longest one.  But if a shorter prefix matches on a soft-terminating
    ;; parser, we want to use that one instead.
    (define hard-delimit-length (and (not (null? hard-delimited-lengths))
                                     (car (reverse hard-delimited-lengths))))
    (define new-soft-tries
      (append (list (cons (chido-readtable-soft-terminating-trie rt) 0)
                    (cons (chido-readtable-soft-terminating-layout-trie rt) 0))
              soft-trie-pairs))
    (define soft-delimit-pairs-to-try
      (reverse
       (filter (λ (tp) (and (not (trie-bare? (car tp)))
                            (or (not hard-delimit-length)
                                (< hard-delimit-length (cdr tp)))))
               new-soft-tries)))
    (define soft-delimit-length
      (for/fold ([delimit-length #f])
                ([soft-pair soft-delimit-pairs-to-try]
                 #:break delimit-length)
        (for/fold ([delimit-length delimit-length])
                  ([parser (trie-values (car soft-pair))]
                   #:break delimit-length)
          (define start-offset (- len (cdr soft-pair)))
          (define soft-result (parse* pb parser
                                      #:start (+ start-pos start-offset)))
          (if (not (parse-failure? soft-result))
              (cdr soft-pair)
              #f))))

    (define delimit-length (if (and soft-delimit-length hard-delimit-length)
                               (max soft-delimit-length hard-delimit-length)
                               (or soft-delimit-length hard-delimit-length)))


    (if delimit-length
        (- len delimit-length)
        (rec/step len new-hard-tries new-soft-tries)))

  (define (rec/step len hard-trie-pairs soft-trie-pairs)
    (define c (port-broker-char pb (+ len start-pos)))
    (if (eof-object? c)
        len
        (rec/main (add1 len)
                  (filter-map (step-trie-pair c) hard-trie-pairs)
                  (filter-map (step-trie-pair c) soft-trie-pairs))))
  (define ((step-trie-pair c) tp)
    (define next-trie (trie-step (car tp) c #f))
    (and next-trie (cons next-trie (add1 (cdr tp)))))

  ;; actual start of parsing symbols and numbers...

  (chido-readtable-populate-cache! rt)
  (define sym/num-length (rec/main 0 '() '()))
  (define str (port-broker-substring pb start-pos sym/num-length))
  (cond
    [(equal? 0 sym/num-length)
     (make-parse-failure
      #:message "Can't parse symbol because delimiter succeeded parsing."
      #:position start-pos)]
    [(memq (string->symbol str) (chido-readtable-symbol-blacklist rt))
     (make-parse-failure #:message (format "Symbol blacklisted: ~s" str))]
    [else
     (let ()
       (define span sym/num-length)
       (define result-func
         (λ (src line column start-position span derivations)

           ;; TODO - check options for whether complex numbers, rational numbers, and numbers of any kind are supported in this readtable...
           (define number (string->number str))
           (define datum (or number (string->symbol str)))
           (define stx
             (datum->syntax #f datum (list src line column start-position span)))
           ;; TODO - use result transformer
           ;datum
           stx
           ))
       (make-parse-derivation result-func #:end (+ start-pos span)))]))

(define (symbol/number-parser rt)
  (proc-parser #:name "symbol/number-parser" (parse-symbol/number-func rt)
               #:promise-no-left-recursion? #t
               #:use-port? #f))


(define ((cached-access accessor) rt)
  (chido-readtable-populate-cache! rt)
  (accessor rt))
(define chido-readtable->symbol (cached-access chido-readtable-symbol-parser))
(define chido-readtable->layout1 (cached-access chido-readtable-layout1-parser))
(define chido-readtable->layout* (cached-access chido-readtable-layout*-parser))
(define chido-readtable->layout+ (cached-access chido-readtable-layout+-parser))
(define chido-readtable->read1 (cached-access chido-readtable-read1-parser))
(define chido-readtable->read* (cached-access chido-readtable-read*-parser))
(define chido-readtable->read+ (cached-access chido-readtable-read+-parser))
(define chido-readtable->read1/layout
  (cached-access chido-readtable-layout*+read1-parser))

(define (make-current-readtable-x-parser extractor name)
  (proc-parser
   #:name name
   (λ (port) (parse* port (extractor (current-chido-readtable))))))
(define current-chido-readtable-read1-parser
  (make-current-readtable-x-parser chido-readtable->read1
                                   "current-chido-readtable-read1-parser"))
(define current-chido-readtable-symbol-parser
  (make-current-readtable-x-parser chido-readtable->symbol
                                   "current-chido-readtable-symbol-parser"))
(define current-chido-readtable-layout*-parser
  (make-current-readtable-x-parser chido-readtable->layout*
                                   "current-chido-readtable-layout*-parser"))
(define current-chido-readtable-layout+-parser
  (make-current-readtable-x-parser chido-readtable->layout+
                                   "current-chido-readtable-layout+-parser"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Operator Stuff

;; TODO -- these predicates and other functions that operate on operators should normalize parsers -- maybe using parser->usable before comparison.  The normalization probably needs to be somewhat lazy, or at least should not be done during readtable construction/extension.  But it can probably be done during the readtable cache setup phase.
(define (infix-operator? readtable parser)
  (dict-has-key? (chido-readtable-infix-operator->associativity readtable) parser))
(define (prefix-operator? readtable parser)
  (member parser (chido-readtable-prefix-operators readtable)))
(define (postfix-operator? readtable parser)
  (member parser (chido-readtable-postfix-operators readtable)))

(define (operator-priority-< readtable lessparser moreparser)
  (chido-readtable-populate-cache! readtable)
  (member moreparser
          (dict-ref
           (chido-readtable-precedence-transitive-greater-relations readtable)
           lessparser
           '())))

(define ((filter-infix-operator-derivation readtab) port derivation)
  #|
  TODO - associativity groups
  TODO - indirect access (IE path to LHS/RHS through derivations, instead of being a direct child)
  |#
  (define dparser (parse-derivation-parser derivation))
  (define lderiv (parse-derivation-left-most-subderivation derivation))
  (define rderiv (parse-derivation-right-most-subderivation derivation))
  (define ldparser (parse-derivation-parser lderiv))
  (define rdparser (parse-derivation-parser rderiv))
  (define filter-out?
    (or
     ;; associativity violation
     (match (dict-ref (chido-readtable-infix-operator->associativity readtab)
                      dparser)
       ['left (parse-derivation-parser? rderiv dparser)]
       ['right (parse-derivation-parser? lderiv dparser)]
       [#f (or (parse-derivation-parser? lderiv dparser)
               (parse-derivation-parser? rderiv dparser))])

     ;; direct precedence violation
     ;; IE low-priority binary operator on either side
     (or (and (infix-operator? readtab ldparser)
              (operator-priority-< readtab ldparser dparser))
         (and (infix-operator? readtab rdparser)
              (operator-priority-< readtab rdparser dparser)))

     ;; direct precedence violation by non-relation
     ;; IE operators don't have a relationship.
     ;; TODO - this code is wrong, because it includes equal precedence...
     #;(or (and (infix-operator? readtab ldparser)
              (not (operator-priority-< readtab ldparser dparser))
              (not (operator-priority-< readtab dparser ldparser)))
         (and (infix-operator? readtab rdparser)
              (not (operator-priority-< readtab rdparser dparser))
              (not (operator-priority-< readtab dparser rdparser))))

     ;; deep precedence violation
     ;; IE
     ;; * low-priority prefix operator on right of left side, recursively
     ;; * low-priority postfix operator on left of right side, recursively
     (let* ([get-neighbor (λ (start next-accessor)
                            (let loop ([next start])
                              (if (infix-operator? readtab
                                                   (parse-derivation-parser next))
                                  (loop (next-accessor next))
                                  next)))]
            [right-of-left (parse-derivation-parser
                            (get-neighbor lderiv
                                          parse-derivation-right-most-subderivation))]
            [left-of-right (parse-derivation-parser
                            (get-neighbor rderiv
                                          parse-derivation-left-most-subderivation))])
       (or
        (and (prefix-operator? readtab right-of-left)
             (operator-priority-< readtab
                                  right-of-left
                                  dparser))
        (and (postfix-operator? readtab left-of-right)
             (operator-priority-< readtab
                                  left-of-right
                                  dparser))))))

  (not filter-out?))


(define ((filter-prefix-operator-derivation readtab) port derivation)
  #|
  TODO - indirect access (IE path to LHS/RHS through derivations, instead of being a direct child)
  |#
  (define dparser (parse-derivation-parser derivation))
  (define rderiv (parse-derivation-right-most-subderivation derivation))
  (define filter-out?
    ;; direct precedence violation
    ;; IE low-priority binary operator on either side
    (and (infix-operator? readtab (parse-derivation-parser rderiv))
         (operator-priority-< readtab (parse-derivation-parser rderiv) dparser)))
  (not filter-out?))

(define ((filter-postfix-operator-derivation readtab) port derivation)
  #|
  TODO - indirect access (IE path to LHS/RHS through derivations, instead of being a direct child)
  |#
  (define dparser (parse-derivation-parser derivation))
  (define lderiv (parse-derivation-left-most-subderivation derivation))
  (define filter-out?
    ;; direct precedence violation
    ;; IE low-priority binary operator on either side
    (and (infix-operator? readtab (parse-derivation-parser lderiv))
         (operator-priority-< readtab (parse-derivation-parser lderiv) dparser)))
  (not filter-out?))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Basic readtable-related parsers

;; TODO - what should the default be?
(define current-chido-readtable (chido-parse-parameter #f))


;; TODO - left and right must be strings
(define (chido-readtable-add-list-parser
         left right rt
         #:wrapper [wrapper #f]
         #:inside-readtable [inside-readtable #f]
         #:readtable-symbol-effect [rt-add-type 'terminating]
         )
  (define (inner-parser)
    (proc-parser #:name (format "list-inner-parser-~a-~a" left right)
                 (λ (port)
                   (define inner-rt (match inside-readtable
                                      [#f (current-chido-readtable)]
                                      [(? chido-readtable?) inside-readtable]
                                      [(? procedure?) (inside-readtable)]))
                   (parse* port (chido-readtable->read* inner-rt)))
                 #:use-port? #f))
  (define left-parser
    (sequence
     left inner-parser right
     ;; TODO - use the optional transformer argument
     ;; TODO - the result should be a syntax object by default
     #:derive (λ derivations
                (make-parse-derivation
                 (λ (src line col pos span derivations)
                   (define pre-result (parse-derivation-result (second derivations)))
                   (define (->stx pre-result)
                     (datum->syntax #f pre-result
                                    (list src line col pos span)))
                   (match wrapper
                     [(? procedure?) (wrapper (->stx pre-result))]
                     [(? symbol?) (datum->syntax
                                   #f
                                   (cons (datum->syntax
                                          #f wrapper
                                          (list src
                                                line col pos
                                                (string-length left)))
                                         (syntax->list pre-result))
                                   pre-result)]
                     [#f (->stx pre-result)]))
                 #:derivations derivations))
     ;#:result (λ (l inner right) inner)
     ))

  (define right-parser
    (proc-parser #:name (format "trailing-right-delimiter_~a" right)
                 #:prefix right
                 (λ (port) (make-parse-failure
                            #:message (format "Trailing right delimiter: ~a"
                                              right)))
                 #:promise-no-left-recursion? #t))

  (extend-chido-readtable
   rt-add-type right-parser
   (extend-chido-readtable rt-add-type left-parser rt)))


(define racket-style-string-parser
  (proc-parser #:name "racket-style-string-parser"
               #:prefix "\""
               (λ (port)
                 (define r (read-syntax (object-name port) port))
                 (define-values (line col pos) (port-next-location port))
                 (make-parse-derivation r #:end pos))
               #:promise-no-left-recursion? #t
               #:preserve-prefix? #t))

(define (mk-stx v derivation)
  (datum->syntax #f v (list (parse-derivation-source-name derivation)
                            (parse-derivation-line derivation)
                            (parse-derivation-column derivation)
                            (parse-derivation-start-position derivation)
                            (- (parse-derivation-end-position derivation)
                               (parse-derivation-start-position derivation)))))

(define hash-t-parser
  (wrap-derivation (follow-filter "#t"
                                  current-chido-readtable-symbol-parser)
                   (λ(x)(mk-stx #t x))))
(define hash-f-parser
  (wrap-derivation (follow-filter "#f"
                                  current-chido-readtable-symbol-parser)
                   (λ(x)(mk-stx #f x))))

(define post-quote-read-1
  (proc-parser
   (λ (port)
     (parse* port (chido-readtable->read1/layout (current-chido-readtable))))))

(define (make-quote-parser prefix quotey-symbol)
  (sequence #:name (symbol->string quotey-symbol)
            prefix
            post-quote-read-1
            #:derive (λ derivations
                       (make-parse-derivation
                        (λ (src line col pos span derivations)
                          (datum->syntax
                           #f
                           (list (mk-stx quotey-symbol (first derivations))
                                 (parse-derivation-result (second derivations)))
                           (list src line col pos span)))
                        #:derivations derivations))))

(define (add-quasi-expression-comment prefix uncomment-prefix rt)
  (define current-quasi-expression-comment-active (chido-parse-parameter #f))
  (define uncomment-parser
    (proc-parser
     #:prefix uncomment-prefix
     (λ (port)
       (if (current-quasi-expression-comment-active)
           (chido-parse-parameterize
            ([current-quasi-expression-comment-active #f])
            (define d (parse*-direct port (current-chido-readtable)))
            (make-parse-derivation (λ args (parse-derivation-result d))
                                   #:derivations d))
           (make-parse-failure
            #:message
            "uncomment parser is only valid inside a quasi-expression-comment")))))
  (define inner-parser
    (proc-parser
     #:prefix prefix
     (λ (port)
       (chido-parse-parameterize ([current-quasi-expression-comment-active #t])
                                 (parse* port (current-chido-readtable))))))
  (define (find-uncomments derivation)
    (cond [(eq? (parse-derivation-parser derivation) uncomment-parser)
           (list derivation)]
          [else (apply append (map find-uncomments
                                   (parse-derivation-subderivations derivation)))]))
  (define (make-quasicomment-parser all-layout?)
    (proc-parser
     #:prefix prefix
     #:preserve-prefix? #t
     #:promise-no-left-recursion? (not (equal? "" prefix))
     (λ (port)
       (define d (parse*-direct port inner-parser))
       (define uncomments (find-uncomments d))
       (cond [(null? uncomments)
              (if all-layout?
                  d
                  (make-parse-failure
                   #:message
                   "quasicomment had no uncomment, so it is entirely layout."))]
             [(null? (cdr uncomments))
              (define uncomment (car uncomments))
              (if all-layout?
                  (make-parse-failure
                   #:message
                   "quasicomment contains uncomment, so it is not entirely layout.")
                  (make-parse-derivation (λ args (parse-derivation-result uncomment))
                                         #:derivations (list d uncomment)))]
             [else (make-parse-failure
                    #:message
                    "quasicomment contained multiple uncomments")]))))
  (define all-layout-parser (make-quasicomment-parser #t))
  (define with-uncomment-parser (make-quasicomment-parser #f))

  (extend-chido-readtable*
   rt
   'nonterminating-layout all-layout-parser
   'nonterminating with-uncomment-parser
   'nonterminating uncomment-parser))

(define (make-line-comment-parser prefix)
  (proc-parser
   #:prefix prefix
   (λ (port)
     (let loop ()
       (define c (peek-char port))
       (if (or (eof-object? c) (eq? c #\newline))
           (let-values ([(line col pos) (port-next-location port)])
             (make-parse-derivation #t #:end pos))
           (begin (read-char port)
                  (loop)))))))

(define (make-keyword-parser prefix)
  (sequence #:name "keyword"
            prefix
            (proc-parser
             (λ (port) (parse* port (chido-readtable->symbol
                                     (current-chido-readtable)))))
            #:derive (λ derivations
                       (make-parse-derivation
                        (λ (src line col pos span derivations)
                          (datum->syntax
                           #f
                           (string->keyword
                            (symbol->string
                             (syntax->datum
                              (parse-derivation-result (second derivations)))))
                           (list src line col pos span)))
                        #:derivations derivations))))

(define (make-raw-string-parser l-delim r-delim #:wrapper [wrapper #f])
  (proc-parser
   #:name "raw-string"
   #:prefix l-delim
   (λ (port)
     (define the-string
       (let loop ([current-depth 1]
                  [left-partials '()]
                  [right-partials '()]
                  [chars '()])
         (define c (read-char port))
         (when (eof-object? c)
           (error 'raw-string (format "Reached EOF before string terminator (~a)"
                                      r-delim)))
         (define matched-right? #f)
         (define new-right-partials
           (filter (λ(x)x)
                   (for/list ([i (cons 0 right-partials)])
                     (define match (eq? c (string-ref r-delim i)))
                     (when (and match (eq? i (sub1 (string-length r-delim))))
                       (set! matched-right? #t))
                     (and match
                          (add1 i)))))
         (define matched-left? #f)
         (define new-left-partials
           (filter (λ(x)x)
                   (for/list ([i (cons 0 left-partials)])
                     (define match (eq? c (string-ref l-delim i)))
                     (when (and match (eq? i (sub1 (string-length l-delim))))
                       (set! matched-left? #t))
                     (and match
                          (add1 i)))))
         ;; TODO - there is a problem here if the left and right delimiters can overlap such that the right delimiter starts before the right delimiter but ends after the left delimiter
         (cond [(and matched-right? (eq? current-depth 1))
                (apply string (reverse
                               (list-tail chars (sub1 (string-length r-delim)))))]
               [matched-right? (loop (sub1 current-depth)
                                     '()
                                     '()
                                     (cons c chars))]
               [matched-left? (loop (add1 current-depth)
                                    '()
                                    '()
                                    (cons c chars))]
               [else (loop current-depth
                           new-left-partials
                           new-right-partials
                           (cons c chars))])))
     (define-values (line col pos) (port-next-location port))

     (make-parse-derivation
      (λ (src line col pos span derivations)
        (define stx (datum->syntax #f the-string (list src line col pos span)))
        (cond [(procedure? wrapper) (wrapper stx)]
              [(symbol? wrapper) (datum->syntax
                                  #f (list (datum->syntax
                                            #f wrapper
                                            (list src line col pos (string-length l-delim)))
                                           stx))]
              [(not wrapper) stx]
              [else (error 'raw-string-parser "bad wrapper value: ~a" wrapper)])
        )
      #:end pos))))

(define (chido-readtable-add-raw-string-parser
         left right rt
         #:wrapper [wrapper #f]
         #:readtable-symbol-effect [rt-add-type 'terminating]
         )
  (define right-parser
    (proc-parser #:name (format "trailing-right-delimiter_~a" right)
                 #:prefix right
                 (λ (port) (make-parse-failure
                            #:message (format "Trailing right delimiter: ~a"
                                              right)))
                 #:promise-no-left-recursion? #t
                 #:use-port? #f))
  (extend-chido-readtable
   rt-add-type right-parser
   (extend-chido-readtable rt-add-type (make-raw-string-parser left right
                                                               #:wrapper wrapper)
                           rt)))


(define (chido-readtable-add-mixfix-operator
         name-strsym rt
         #:layout [layout 'required]
         #:precedence-greater-than [precedence-greater-than '()]
         #:precedence-less-than [precedence-less-than '()]
         #:associativity [associativity #f])
  (define name-str (cond [(string? name-strsym) name-strsym]
                         [(symbol? name-strsym) (symbol->string name-strsym)]
                         [else (error 'TODO-better-message)]))
  (define name-split (string-split name-str "_" #:trim? #f))
  (when (string-contains? name-str "__")
    (error 'chido-readtable-add-mixfix-operator
           "don't use double underscore in the operator name/spec: ~v"
           name-str))
  (when (not (string-contains? name-str "_"))
    (error 'chido-readtable-add-mixfix-operator
           "didn't include any underscores in operator name/spec: ~v"
           name-str))
  (when (<= (length name-split) 1)
    (error 'chido-readtable-add-mixfix-operator
           "name/spec was too short: ~v"
           name-str))

  (define parsers/no-layout (map (λ (s) (if (equal? "" s)
                                            current-chido-readtable-read1-parser
                                            s))
                                 name-split))
  (define parsers
    (match layout
      ['required (flatten
                  (cons (car parsers/no-layout)
                        (map (λ (p) (list current-chido-readtable-layout+-parser p))
                             (cdr parsers/no-layout))))]
      ['optional (flatten
                  (cons (car parsers/no-layout)
                        (map (λ (p) (list current-chido-readtable-layout*-parser p))
                             (cdr parsers/no-layout))))]
      ['none parsers/no-layout]))

  (define result-indices/no-layout
    (filter (λ(x)x)
            (for/list ([part name-split]
                       [i (in-naturals)])
              (and (equal? part "") i))))
  (define result-indices
    (match layout
      ['none result-indices/no-layout]
      [else (map (λ (x) (if (equal? 0 x)
                            0
                            (* x 2)))
                 result-indices/no-layout)]))

  (define postfix? (string-prefix? name-str "_"))
  (define prefix? (string-suffix? name-str "_"))
  (define operator-style
    (cond [(and prefix? postfix?) 'infix]
          [prefix? 'prefix]
          [postfix? 'postfix]
          [else #f]))

  (define operator-name-use
    (match operator-style
      [#f name-str]
      ['postfix (substring name-str 1)]
      ['prefix (substring name-str 0 (sub1 (string-length name-str)))]
      ['infix (substring name-str 1 (sub1 (string-length name-str)))]))

  (define symbols-to-blacklist (filter-map (λ (x) (and (not (equal? "" x))
                                                       (string->symbol x)))
                                           name-split))

  (define sequence-parser
    (apply
     sequence
     #:name operator-name-use
     #:derive (λ derivations
                (make-parse-derivation
                 (λ (src line col pos span derivations)
                   (datum->syntax
                    #f
                    `(,(match operator-style
                         ['infix '#%chido-readtable-infix-operator]
                         ['prefix '#%chido-readtable-prefix-operator]
                         ['postfix '#%chido-readtable-postfix-operator]
                         [#f '#%chido-readtable-notfix-operator])
                      ,(string->symbol operator-name-use)
                      ,@(map (λ (i) (parse-derivation-result
                                     (list-ref derivations i)))
                             result-indices))
                    (list src line col pos span)))
                 #:derivations derivations))
     parsers))

  (extend-chido-readtable 'left-recursive-nonterminating
                          #:precedence-greater-than precedence-greater-than
                          #:precedence-less-than precedence-less-than
                          #:associativity associativity
                          #:symbol-blacklist symbols-to-blacklist
                          #:operator operator-style
                          sequence-parser
                          rt))

(define-syntax (chido-readtable-add-mixfix-operators stx)
  (syntax-parse stx
    [(_
      [name:id (~or (~optional (~seq #:layout layout-arg:id))
                    (~optional (~seq #:precedence-greater-than [pgt:id ...]))
                    (~optional (~seq #:precedence-less-than [plt:id ...]))
                    (~optional (~seq #:associativity (~or assoc-arg:id #f))))
               ...]
      ...
      start-readtable:expr)
     #'(let loop ([rt start-readtable]
                  [op-specs (list
                             (list 'name
                                   '(~? layout-arg required)
                                   (~? '(pgt ...) '())
                                   (~? '(plt ...) '())
                                   '(~? assoc-arg #f))
                             ...)])
         (match op-specs
           ['() rt]
           [(list-rest (list op-name layout gts lts assoc) more)
            (loop (chido-readtable-add-mixfix-operator
                   op-name
                   rt
                   #:layout layout
                   #:associativity assoc
                   #:precedence-greater-than gts
                   #:precedence-less-than lts)
                  more)]))]))


(define (->symbol symstr)
  (match symstr
    [(? string?) (string->symbol symstr)]
    [(? symbol?) symstr]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Testing
(module+ test
  (require rackunit)
  (require racket/stream)
  (require "test-util-3.rkt")
  (define (make-<two>-parser)
    (proc-parser (λ (port)
                   (define s (read-string 5 port))
                   (define-values (line col pos) (port-next-location port))
                   (if (equal? s "<two>")
                       (make-parse-derivation s #:end pos)
                       (make-parse-failure #:message "didn't match «<two>»")))))
  (define (make-<two/alt>-parser)
    (proc-parser (λ (port)
                   (define s (read-string 9 port))
                   (define-values (line col pos) (port-next-location port))
                   (if (equal? s "<two/alt>")
                       (stream-cons (make-parse-derivation s #:end pos)
                                    (stream-cons (make-parse-derivation s #:end pos)
                                                 empty-stream))
                       (make-parse-failure #:message "didn't match «<two>»")))))

  (define my-rt
    (extend-chido-readtable*
     (add-quasi-expression-comment
      "##`;" "##,;"
      (chido-readtable-add-mixfix-operators
       [_<^>_ #:associativity right]
       [_<*>_ #:associativity left
              #:precedence-greater-than [<+>]
              #:precedence-less-than [<^>]]
       [<low-prefix>_ #:precedence-less-than [<+>]]
       [_<low-postfix> #:precedence-less-than [<+>]]
       (chido-readtable-add-mixfix-operator
        "_<+>_" #:associativity 'left
        (chido-readtable-add-raw-string-parser
         "!!" "!!"
         (chido-readtable-add-raw-string-parser
          "<<" ">>"
          (chido-readtable-add-list-parser
           "##{" "}##" #:readtable-symbol-effect 'terminating-layout
           (chido-readtable-add-raw-string-parser
            "#|" "|#" #:readtable-symbol-effect 'terminating-layout
            (chido-readtable-add-list-parser
             "$(" ")" #:wrapper '#%dollar-paren
             (chido-readtable-add-list-parser
              "[" "]"
              (chido-readtable-add-list-parser "(" ")" empty-chido-readtable))))))))))
     'terminating "##"
     'terminating racket-style-string-parser
     'nonterminating hash-t-parser
     'nonterminating hash-f-parser
     'terminating (make-quote-parser "'" 'quote)
     'terminating (make-quote-parser "`" 'quasiquote)
     'terminating (make-quote-parser (follow-filter "," "@") 'unquote)
     'terminating (make-quote-parser ",@" 'unquote-splicing)
     'terminating (make-quote-parser "#'" 'syntax)
     'terminating (make-quote-parser "#`" 'quasisyntax)
     'terminating (make-quote-parser (follow-filter "#," "@") 'unsyntax)
     'terminating (make-quote-parser "#,@" 'unsyntax-splicing)
     'terminating (make-keyword-parser "#:")
     ;; two of these, to test that I'm getting sequences back properly
     'nonterminating (make-<two>-parser)
     'nonterminating (make-<two>-parser)
     'nonterminating (make-<two/alt>-parser)
     'terminating-layout " "
     'terminating-layout "\n"
     'terminating-layout "\t"
     ;; creative use of string-append to get emacs to color properly...
     'nonterminating-layout (make-quote-parser (string-append "#" ";") 'comment-quote)
     'terminating-layout (make-line-comment-parser ";")
     )
    )

  (define (p*/d string parser)
    (define r (with-handlers ([(λ(e)#t)(λ(e)e)])
                (parse* (open-input-string string) parser)))
    (if (or (parse-failure? r)
            (exn? r))
        r
        (map (λ (x) (if (syntax? x) (syntax->datum x) x))
             (map parse-derivation-result
                  (stream->list
                   r)))))

  (define r1 (chido-readtable->read1 my-rt))
  (define r1/no-symbol
    (chido-readtable->read1 (set-chido-readtable-symbol-support my-rt #f)))

  (chido-parse-parameterize
   ([current-chido-readtable my-rt])

   (check-pred (λ(x) (and (not (parse-failure? x))
                          (list? x)
                          (< 0 (length x))))
               (p*/d "   \t\n  " (chido-readtable-layout*-parser my-rt)))
   (check se/datum?
          (p*/r "()" r1)
          (list #'()))
   (check se/datum? (p*/r "( )" r1) (list #'()))
   (check se/datum? (p*/r "( ( ))" r1) (list #'(())))
   (check se/datum? (p*/r "( ( ) )" r1) (list #'(())))
   (check se/datum? (p*/r "( ( ) )" r1) (list #'(())))
   (check se/datum? (p*/r "(()()()()())" r1) (list #'(()()()()())))
   (check-pred parse-failure? (p*/d ")" r1))
   (check se/datum? (p*/r "testing" r1) (list #'testing))
   (check se/datum? (p*/r "(testing)" r1) (list #'(testing)))
   (check se/datum? (p*/r "( testing)" r1) (list #'(testing)))
   (check se/datum? (p*/r "(testing )" r1) (list #'(testing)))
   (check se/datum? (p*/r "( testing )" r1) (list #'(testing)))
   (check se/datum? (p*/r "( testing #|in comment here|# foo)" r1)
          (list #'(testing foo)))
   (check se/datum? (p*/r "( testing ##{testing (a b c) foo}## foo)" r1)
          (list #'(testing foo)))
   (check se/datum? (p*/r "( testing ##`;(testing a quasicomment with ##,;(uncommenting) capabilities) foo)" r1)
          (list #'(testing (uncommenting) foo)))
   (check-pred parse-failure?
               (parse* (open-input-string "(here ##{w/ unbalanced (parens}## after)")
                       r1))
   (define s1 "(hello ( goodbye () ( ( ) ) ) bandicoot kangaroo ( aardvark   ))")
   (check se/datum?
          (p*/r s1 r1)
          (list (read-syntax 'foo (open-input-string s1))))
   ;; Check that using the readtable directly is the same as ->read1
   (check se/datum?
          (p*/r s1 my-rt)
          (list (read-syntax 'foo (open-input-string s1))))
   (check se/datum?
          (p*/r "\"testing 123\"" r1)
          (list #'"testing 123"))
   (check se/datum?
          (p*/r "(this is \"a test\" of string reading)" r1)
          (list #'(this is "a test" of string reading)))

   (check se/datum?
          (p*/r "(this (is \"a test\") of string reading)" r1)
          (list #'(this (is "a test") of string reading)))

   (check se/datum?
          (p*/r "(this (is <<a test <<of raw>> string>>) reading)" r1)
          (list #'(this (is "a test <<of raw>> string") reading)))
   ;; If the left and right delimiters are the same, a raw string is not nestable.
   (check se/datum?
          (p*/r "(this (is !!a test !!of raw!! string!!) reading)" r1)
          (list #'(this (is "a test " of raw " string") reading)))
   ;; this one had a weird error at one point.  I'm not sure what it was, but I'm leaving this here.
   (check-not-exn (λ () (p*/d "   \n   " (chido-readtable-layout*-parser my-rt))))

   (check se/datum?
          (p*/r "(hello `(foo ,bar ,(#:testing #t #f #;(quoted #t))))" r1)
          (list #'(hello `(foo ,bar ,(#:testing #t #f)))))

   (check se/datum?
          (p*/r "`(hello ,@foo)" r1)
          (list #'`(hello ,@foo)))

   (check se/datum?
          (p*/r "$(test)" r1)
          (list #'(#%dollar-paren test)))
   (check se/datum?
          (p*/r "$(test foo (bar $(qwer)))" r1)
          (list #'(#%dollar-paren test foo (bar (#%dollar-paren qwer)))))
   (check se/datum?
          (p*/r "[a b]" r1)
          [list #'(a b)])

   (check se/datum?
          (p*/r "(a <two> b)" r1)
          [list #'(a "<two>" b) #'(a "<two>" b)])
   (check se/datum?
          (p*/r "(a <two/alt> b)" r1)
          [list #'(a "<two/alt>" b) #'(a "<two/alt>" b)])

   ;;; operators
   (check se/datum?
          (p*/r "[() <+> ()]" r1)
          [list #'((#%chido-readtable-infix-operator <+> () ()))])
   (check se/datum?
          (p*/r "[() <+> () <+> ()]" r1)
          [list #'((#%chido-readtable-infix-operator
                    <+> (#%chido-readtable-infix-operator <+> () ())
                    ()))])
   (check se/datum?
          (p*/r "[() <^> () <^> ()]" r1)
          [list #'((#%chido-readtable-infix-operator
                    <^> () (#%chido-readtable-infix-operator <^> () ())))])
   (check se/datum?
          (p*/r "[() <+> () <*> ()]" r1)
          [list #'((#%chido-readtable-infix-operator <+> () (#%chido-readtable-infix-operator <*> () ())))])
   (check se/datum?
          (p*/r "[() <*> () <+> ()]" r1)
          [list #'((#%chido-readtable-infix-operator <+> (#%chido-readtable-infix-operator <*> () ()) ()))])
   (check se/datum?
          (p*/r "[() <+> () <^> ()]" r1)
          [list #'((#%chido-readtable-infix-operator <+> () (#%chido-readtable-infix-operator <^> () ())))])
   (check se/datum?
          (p*/r "[() <^> () <+> ()]" r1)
          [list #'((#%chido-readtable-infix-operator <+> (#%chido-readtable-infix-operator <^> () ()) ()))])


   (check se/datum?
          (p*/r "[<low-prefix> a 1 2 3]" r1)
          [list #'((#%chido-readtable-prefix-operator <low-prefix> a) 1 2 3)])
   (check se/datum?
          (p*/r "[(testing 123) <+> (foo (bar))]" r1)
          [list #'((#%chido-readtable-infix-operator <+> (testing 123) (foo (bar))))])

   (check se/datum?
          (p*/r "[a <+> b 1 2 3]" r1)
          [list #'((#%chido-readtable-infix-operator <+> a b) 1 2 3)])
   (check se/datum?
          (p*/r "[a <low-postfix>]" r1)
          [list #'((#%chido-readtable-postfix-operator <low-postfix> a))])
   (check se/datum?
          (p*/r "[1 <+> 2 <+> 3 <+> 4]" r1)
          [list #'((#%chido-readtable-infix-operator
                    <+>
                    (#%chido-readtable-infix-operator <+>
                                                      (#%chido-readtable-infix-operator <+> 1 2)
                                                      3)
                    4))])
   (check se/datum?
          (p*/r "[1 <+> 2 <*> 3]" r1)
          [list #'((#%chido-readtable-infix-operator <+> 1 (#%chido-readtable-infix-operator <*> 2 3)))])
   (check se/datum?
          (p*/r "[1 <*> 2 <+> 3]" r1)
          [list #'((#%chido-readtable-infix-operator <+> (#%chido-readtable-infix-operator <*> 1 2) 3))])

   ;;; operator deep precedence issue
   (check se/datum?
          (p*/r "[#t <+> <low-prefix> #f <+> #t]" r1)
          [list #'((#%chido-readtable-infix-operator
                    <+>
                    #t
                    (#%chido-readtable-prefix-operator
                     <low-prefix> (#%chido-readtable-infix-operator <+> #f #t))))])
   (check se/datum?
          (p*/r "[#f <+> #t <low-postfix> <+> #f]" r1)
          [list #'((#%chido-readtable-infix-operator
                    <+>
                    (#%chido-readtable-postfix-operator
                     <low-postfix> (#%chido-readtable-infix-operator <+> #f #t))
                    #f))])

   ;; check symbol blacklist that operators were added to
   (check-pred parse-failure?
               (p*/d "<+>" r1))

   (check se/datum?
          (p*/r "some-symbol" r1)
          [list #'some-symbol])
   (check-pred parse-failure?
               (p*/d "some-symbol" r1/no-symbol))

   )
  )

#|
TODO
Make submodules providing some pre-made readtables:
- a minimal s-exp reader with few features
- racket default readtable re-implementation
- a default chido-parse readtable with good improvements
  - simplification of some complicated parts (eg. numbers, #stuff)
  - nice features like raw strings, quasi-expression-comment, etc
|#

(module+ an-s-exp-readtable
  (provide an-s-exp-readtable)
  (define an-s-exp-readtable
    (extend-chido-readtable*
     (chido-readtable-add-list-parser
      "##{" "}##" #:readtable-symbol-effect 'terminating-layout
      (chido-readtable-add-raw-string-parser
       "#|" "|#" #:readtable-symbol-effect 'terminating-layout
       (chido-readtable-add-raw-string-parser
        "«" "»"
        (chido-readtable-add-list-parser
         "{" "}"
         (chido-readtable-add-list-parser
          "[" "]"
          (chido-readtable-add-list-parser "(" ")" empty-chido-readtable))))))
     'nonterminating hash-t-parser
     'nonterminating hash-f-parser
     'terminating racket-style-string-parser
     'terminating (make-quote-parser "'" 'quote)
     'terminating (make-quote-parser "`" 'quasiquote)
     'terminating (make-quote-parser (follow-filter "," "@") 'unquote)
     'terminating (make-quote-parser ",@" 'unquote-splicing)
     'terminating (make-quote-parser "#'" 'syntax)
     'terminating (make-quote-parser "#`" 'quasisyntax)
     'terminating (make-quote-parser (follow-filter "#," "@") 'unsyntax)
     'terminating (make-quote-parser "#,@" 'unsyntax-splicing)
     'terminating (make-keyword-parser "#:")
     'terminating-layout " "
     'terminating-layout "\n"
     'terminating-layout "\t"
     ;; creative use of string-append to get emacs to color properly...
     'nonterminating-layout (make-quote-parser (string-append "#" ";")'comment-quote)
     'terminating-layout (make-line-comment-parser ";")
     ))

  (module+ read-syntax-proc
    (require racket/stream racket/port)
    (provide read-syntax-proc)
    (define read-syntax-proc
      (λ (src-name port)
        (port-count-lines! port)
        (define result
          (whole-parse* port (chido-readtable->read* an-s-exp-readtable)))
        #;(eprintf "results: ~a\n" (car (map parse-derivation-result (stream->list result))))
        (cond [(parse-failure? result) (error 'my-read "parse failure: ~s\n" result)]
              [(stream-empty? result)
               (error 'my-read "parse error: ~s\n" result)]
              [(stream-empty? (stream-rest result))
               ;; This is what we want.
               (define out
                 (parse-derivation-result (stream-first result)))
               ;(eprintf "out: ~a\n" out)
               (port->string port)
               #;(datum->syntax #f (cons '#%module-begin out)
                                (list src-name #f #f #f #f))
               ;; consume the port -- not doing so seems to cause issues.
               out]
              [else
               (eprintf "num parses: ~a\n" (length (stream->list result)))
               (eprintf "parses:\n ~s\n" (map parse-derivation-result
                                              (stream->list result)))
               (error 'my-read "ambiguous parse")])))
    )
  )
