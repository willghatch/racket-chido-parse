#lang racket/base

#|
Arguably the point of Racket's readtable is to have an extensible alternate parser that has a built-in symbol parser that is affected by each added alternate.
This is an implementation of the same idea, but also adding support for operators with declarative precidence and associativity and potentially other extra features.
|#

(require racket/contract/base)
(provide
 ;; TODO - the names provided should maybe be chido-readtable-* ...
 (contract-out
  ;; TODO - re-think what the empty readtable should be...
  [empty-chido-readtable chido-readtable?]
  [extend-chido-readtable (-> chido-readtable?
                              (or/c 'terminating 'soft-terminating
                                    'nonterminating 'layout
                                    'left-recursive-nonterminating)
                              any/c
                              chido-readtable?)]
  [chido-readtable-add-list-parser (-> chido-readtable?
                                       string? string?
                                       chido-readtable?)]
  [chido-readtable->read1 (-> chido-readtable? any/c)]
  [chido-readtable->read1/layout (-> chido-readtable? any/c)]
  [chido-readtable->read* (-> chido-readtable? any/c)]
  [chido-readtable->read+ (-> chido-readtable? any/c)]
  [chido-readtable->layout1 (-> chido-readtable? any/c)]
  [chido-readtable->layout* (-> chido-readtable? any/c)]
  [chido-readtable->layout+ (-> chido-readtable? any/c)]
  [set-chido-readtable-symbol-support (-> chido-readtable? any/c chido-readtable?)]
  [chido-readtable-symbol-support? (-> chido-readtable? boolean?)]
  [chido-readtable-add-mixfix-operator
   (->* (chido-readtable? (or/c symbol? string?))
        (#:layout (or/c 'required 'optional 'none)
         #:precidence-greater-than (listof any/c)
         #:precidence-less-than (listof any/c)
         #:associativity (or/c 'left 'right #f))
        any/c)]
  )
 extend-chido-readtable*
 chido-readtable-add-mixfix-operators
 current-chido-readtable

 ;; TODO - maybe not these?
 hash-t-parser
 hash-f-parser
 racket-style-string-parser

 )

(require
 "scheduler.rkt"
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
   ;; Core parsers
   terminating-parsers
   soft-terminating-parsers
   nonterminating-parsers
   left-recursive-nonterminating-parsers
   layout-parsers

   ;;; Options

   ;; If symbol support is off, then terminating, soft-terminating, and nonterminating parsers are indistinguishable.  The difference is only how it affects the built-in symbol parser.
   ;; For readtable-style extension, symbols need to be built-in so adding parsers affects the symbol parser implicitly.
   ;; TODO - I should add more ways to affect the built-in symbol parser.  Eg. maybe soft-terminating-failure and nonterminating-failure parsers, which delimit symbols a la soft-terminating and nonterminating, making the symbol parser fail or not run when they succeed, but that don't create a derivation.  IE they make the symbol parser fail with a message saying that the other parser succeeded.  Basically programatic filters for symbols beyond just having a blacklist.  Eg. this would allow a convention for operator names (eg. they must be in <> like <+>, or they must be in : like :+: or :where:, etc, then fail symbols that match that pattern.)
   ;; TODO - operator precidence programability -- if you have an operator family, such as an operator that reads the operator string as some family of symbols like :+:, :where:, etc, does it make sense to have some more complicated and programmable way of doing precidence and associativity?
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

   ;; TODO - add numeric precidence in addition to relative precidence.  Then users can choose one way or the other.  If both are used, make them separate -- IE compare precidence based on lattice and based on number, don't make the number operator transitive with the lattice operator.  Maybe also error if there is a relationship both ways?  Or maybe error when a readtable has both numeric and relative precidence?
   ;; TODO - the current design makes all precidence relationships transitive.  Maybe both transitive and intransitive relationships should be allowed?
   ;; hash from parser to set of parsers that are immediately above the key parser in the precidence lattice
   precidence-immediate-greater-relations
   ;; hash from parser to set of parsers that are immediately below the key parser in the precidence lattice
   precidence-immediate-lesser-relations

   ;; lists of parsers that are marked as these things
   prefix-operators
   postfix-operators
   ;; dict of binary operator to its associativity
   infix-operator->associativity
   ;; dict of operator names to the parser objects
   operator-name->operator


   ;; Basically these are cached results
   [flush-state? #:mutable]
   [terminating-trie #:mutable]
   [soft-terminating-trie #:mutable]
   [nonterminating-trie #:mutable]
   [layout-trie #:mutable]

   ;; These exist to ensure parsers created with chido-readtable are always eq?
   [symbol-parser #:mutable]
   [layout1-parser #:mutable]
   [layout*-parser #:mutable]
   [layout+-parser #:mutable]
   [read1-parser #:mutable]
   [layout*+read1-parser #:mutable]
   [read*-parser #:mutable]
   [read+-parser #:mutable]

   ;; To cache precidence lattice traversal and cycle detection
   [precidence-transitive-greater-relations #:mutable]
   )
 ; #:prop prop:custom-parser
 ; (λ (self)
 ;   ;; return a parser object
 ;   TODO)
  )


(define empty-chido-readtable
  (chido-readtable
   ;; core parsers
   '() '() '() '() '()

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

   ;; precidence-immediate-greater-relations
   (hash)
   ;; precidence-immediate-lesser-relations
   (hash)
   ;; prefix-operators
   '()
   ;; postfix-operators
   '()
   ;; infix-operator->associativity
   (hash)
   ;; operator-name->operator
   (hash)

   ;;; cached stuff

   ;; flush-state?
   #t
   ;; tries
   empty-trie
   empty-trie
   empty-trie
   empty-trie
   ;; parsers
   #f #f #f #f #f #f #f #f
   ;; precidence-transitive-greater-relations
   (hash)
   ))

(define (extend-chido-readtable rt extension-type parser
                                ;; TODO - better name for extension-type, and make it a keyword argument.
                                #:operator [operator #f]
                                #:precidence-less-than [precidence-less-than '()]
                                #:precidence-greater-than [precidence-greater-than '()]
                                #:associativity [associativity #f]
                                #:symbol-blacklist [symbol-blacklist #f]
                                )
  ;; extension-type is 'terminating, 'soft-terminating, 'nonterminating, 'left-recursive-nonterminating,  or 'layout
  ;; operator is #f, 'infix, 'prefix, or 'postfix
  ;; associativity is #f, 'left, or 'right
  ;; precidence lists are for names of other operators that are immediately greater or lesser in the precidence lattice.
  ;; symbol-blacklist can be #f to do nothing, #t to add the parser name to the symbol blacklist (for the common case that the operator name is the parser name), or a list of symbols or strings to blacklist.
  ;; TODO - maybe symbol-blacklist doesn't belong here, but its primary motivation is to blacklist operator names...

  #| TODO - make extension-type a keyword argument, make the default nonterminating? |#
  (when (and (eq? 'layout extension-type)
             operator)
    (error 'extend-chido-readtable
           "can't add operator parsers to layout parsers"))
  (when (and (member operator '(infix postfix))
             (not (eq? extension-type 'left-recursive-nonterminating)))
    (error 'extend-chido-readtable
           "infix and postfix operators must be added as left-recursive-nonterminating parsers"))

  (define pre-blacklist
    (match extension-type
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
         (cons parser (chido-readtable-left-recursive-nonterminating-parsers rt))])]
      ['layout
       (struct-copy
        chido-readtable
        rt
        [layout-parsers
         (cons parser (chido-readtable-layout-parsers rt))]
        [flush-state? #t])]))
  (define (->symbol symstr)
    (match symstr
      [(? string?) (string->symbol symstr)]
      [(? symbol?) symstr]))
  (define pre-op
    (match symbol-blacklist
      [#f pre-blacklist]
      [#t (struct-copy chido-readtable pre-blacklist
                       [symbol-blacklist (cons (->symbol (parser-name parser))
                                               (chido-readtable-symbol-blacklist
                                                pre-blacklist))])]
      [(list strsym ...)
       (struct-copy chido-readtable pre-blacklist
                    [symbol-blacklist (append (map ->symbol strsym)
                                              (chido-readtable-symbol-blacklist
                                               pre-blacklist))])]))

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
       [precidence-immediate-greater-relations
        (dict-set
         (chido-readtable-precidence-immediate-greater-relations rt)
         parser
         (map op-resolve precidence-less-than))]
       [precidence-immediate-lesser-relations
        (dict-set
         (chido-readtable-precidence-immediate-lesser-relations rt)
         parser
         (map op-resolve precidence-greater-than))]
       [operator-name->operator
        (dict-set op-name->op (parser-name parser) parser)])
      pre-op))

(define (extend-chido-readtable* rt . args)
  (match args
    [(list type parser rest-args ...)
     (apply extend-chido-readtable*
            (extend-chido-readtable rt type parser)
            rest-args)]
    [(list) rt]
    [else (error 'extend-chido-readtable* "bad number of arguments")]))

(define (set-chido-readtable-symbol-support rt bool)
  (struct-copy chido-readtable rt
               [symbol-support? (not (not bool))]
               [flush-state? #t]))

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

    (set-chido-readtable-terminating-trie!
     rt
     (parser-list->trie terminating/wrap))
    (set-chido-readtable-soft-terminating-trie!
     rt
     (parser-list->trie soft-terminating/wrap))
    (set-chido-readtable-nonterminating-trie!
     rt
     (parser-list->trie nonterminating/wrap))
    (set-chido-readtable-symbol-parser! rt (symbol/number-parser rt))
    (set-chido-readtable-layout-trie!
     rt
     (parser-list->trie (chido-readtable-layout-parsers rt)))
    (set-chido-readtable-layout1-parser!
     rt (make-alt-parser "chido-readtable-layout1"
                         (chido-readtable-layout-parsers rt)))
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
      #:name "chido-readtable-read1"
      (let* ([parsers (append
                       nonterminating/wrap
                       soft-terminating/wrap
                       terminating/wrap)]
             [alt (make-alt-parser "chido-readtable-read1/alt"
                                   parsers)]
             [left-recursive-nonterminating-alt
              (make-alt-parser "chido-readtable-read1/lrn-alt"
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
                  (if (parse-failure? symbol-result)
                      ;; TODO - I want a failure that encapsulates the symbol AND parsers results
                      parsers-result
                      symbol-result))]
               [else parsers-result]))
           ;; TODO - use a stream-append that preserves failures!!
           (parse-stream-cons
            symbol-result
            (parse* port left-recursive-nonterminating-alt)))))
      #:use-port? #f
      #:promise-no-left-recursion?
      (not (ormap parser-potentially-left-recursive?
                  (append nonterminating/wrap
                          soft-terminating/wrap
                          terminating/wrap
                          (chido-readtable-layout-parsers rt))))))
    (set-chido-readtable-layout*+read1-parser!
     rt
     (sequence
      (chido-readtable-layout*-parser rt)
      (chido-readtable-read1-parser rt)
      #:derive (λ derivations
                 (make-parse-derivation
                  (λ (line col pos end-pos derivations)
                    (parse-derivation-result (second derivations)))
                  #:derivations derivations))))

    (let ([with-content-parser
            (sequence
             #:name "chido-readtable-read+"
             (kleene-plus (chido-readtable-layout*+read1-parser rt) #:greedy? #t)
             (chido-readtable-layout*-parser rt)
             #:derive (λ derivations
                        (make-parse-derivation
                         (λ (line col pos end-pos derivations)
                           (parse-derivation-result (first derivations)))
                         #:derivations derivations)))]
          [no-content-parser (chido-readtable-layout*-parser rt)])
      (set-chido-readtable-read+-parser! rt with-content-parser)
      (set-chido-readtable-read*-parser!
       rt
       (make-alt-parser "chido-readtable-read*"
                        (list with-content-parser no-content-parser))))

    ;;; Set transitive operator precidence hash

    (define all-direct-greaters
      ;; Because precidence can be declared by a greater than or less than relation,
      ;; I need to reverse one side to get all one relation.
      (for/fold ([ghash (chido-readtable-precidence-immediate-greater-relations rt)])
                ([op (append (chido-readtable-prefix-operators rt)
                             (chido-readtable-postfix-operators rt)
                             (dict-keys
                              (chido-readtable-infix-operator->associativity rt)))])
        (for/fold ([ghash ghash])
                  ([lesser-op
                    (dict-ref
                     (chido-readtable-precidence-immediate-lesser-relations rt)
                     op)])
          (dict-set ghash lesser-op (cons op (dict-ref ghash lesser-op '()))))))
    (define (compute-transitive-greaters orig-op work-set done-set greater-set)
      (if (set-empty? work-set)
          (if (set-member? greater-set orig-op)
              (error 'chido-readtable
                     "Circular operator precidence detected for: ~a"
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
    (set-chido-readtable-precidence-transitive-greater-relations!
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
    (define new-hard-tries (cons (cons (chido-readtable-terminating-trie rt) 0)
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
                    (cons (chido-readtable-layout-trie rt) 0))
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
     (make-parse-failure "Can't parse symbol because delimiter succeeded parsing."
                         #:position start-pos)]
    [(memq (string->symbol str) (chido-readtable-symbol-blacklist rt))
     (make-parse-failure (format "Symbol blacklisted: ~s" str))]
    [else
     (let ()
       (define span sym/num-length)
       (define result-func
         (λ (line column start-position end-position derivations)

           ;; TODO - check options for whether complex numbers, rational numbers, and numbers of any kind are supported in this readtable...
           (define number (string->number str))
           (define datum (or number (string->symbol str)))
           (define stx
             (datum->syntax #f datum (list "TODO-need-port-name-here"
                                           line column start-position
                                           (- end-position start-position))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Operator Stuff

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
           (chido-readtable-precidence-transitive-greater-relations readtable)
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

     ;; direct precidence violation
     ;; IE low-priority binary operator on either side
     (or (and (infix-operator? readtab ldparser)
              (operator-priority-< readtab ldparser dparser))
         (and (infix-operator? readtab rdparser)
              (operator-priority-< readtab rdparser dparser)))

     ;; direct precidence violation by non-relation
     ;; IE operators don't have a relationship.
     ;; TODO - this code is wrong, because it includes equal precidence...
     #;(or (and (infix-operator? readtab ldparser)
              (not (operator-priority-< readtab ldparser dparser))
              (not (operator-priority-< readtab dparser ldparser)))
         (and (infix-operator? readtab rdparser)
              (not (operator-priority-< readtab rdparser dparser))
              (not (operator-priority-< readtab dparser rdparser))))

     ;; deep precidence violation
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
    ;; direct precidence violation
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
    ;; direct precidence violation
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
         rt left right
         #:wrapper [wrapper #f]
         ;#:inside-readtable
         ;; TODO - what is the right name here?
         #:readtable-add-type [rt-add-type 'terminating]
         )
  (define (inner-parser)
    (proc-parser #:name (format "list-inner-parser-~a-~a" left right)
                 (λ (port)
                   (define inner-rt (current-chido-readtable))
                   (parse* port (chido-readtable->read* inner-rt)))
                 #:use-port? #f))
  (define left-parser
    (sequence
     left inner-parser right
     ;; TODO - use the optional transformer argument
     ;; TODO - the result should be a syntax object by default
     #:derive (λ derivations
                (make-parse-derivation
                 (λ (line col pos end-pos derivations)
                   (define pre-result (parse-derivation-result (second derivations)))
                   (define (->stx pre-result)
                     (datum->syntax #f pre-result
                                    (list "TODO-need-port-name-here"
                                          line col pos (- end-pos pos))))
                   (match wrapper
                     [(? procedure?) (wrapper (->stx pre-result))]
                     [(? symbol?) (->stx (cons (datum->syntax
                                                #f wrapper
                                                (list "TODO-need-port-name-here"
                                                      line col pos
                                                      (string-length left)))
                                               pre-result))]
                     [#f (->stx pre-result)]))
                 #:derivations derivations))
     ;#:result (λ (l inner right) inner)
     ))

  (define right-parser
    (proc-parser #:name (format "trailing-right-delimiter_~a" right)
                 #:prefix right
                 (λ (port) (make-parse-failure
                            (format "Trailing right delimiter: ~a"
                                    right)))
                 #:promise-no-left-recursion? #t
                 #:use-port? #f))

  (extend-chido-readtable (extend-chido-readtable rt rt-add-type left-parser)
                          rt-add-type right-parser))


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
  (datum->syntax #f v (list "TODO-need-port-name-here"
                            (parse-derivation-line derivation)
                            (parse-derivation-column derivation)
                            (parse-derivation-start-position derivation)
                            (- (parse-derivation-end-position derivation)
                               (parse-derivation-start-position derivation)))))

(define hash-t-parser (wrap-derivation "#t" (λ(x)(mk-stx #t x))))
(define hash-f-parser (wrap-derivation "#f" (λ(x)(mk-stx #f x))))

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
                        (λ (line col pos end-pos derivations)
                          (datum->syntax
                           #f
                           (list (mk-stx quotey-symbol (first derivations))
                                 (parse-derivation-result (second derivations)))
                           (list "TODO-need-port-name-here"
                                 line col pos (- end-pos pos))))
                        #:derivations derivations))))

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
                        (λ (line col pos end-pos derivations)
                          (datum->syntax
                           #f
                           (string->keyword
                            (symbol->string
                             (syntax->datum
                              (parse-derivation-result (second derivations)))))
                           (list "TODO-need-port-name-here"
                                 line col pos (- end-pos pos))))
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
      (λ (line col pos end-pos derivations)
        (define stx (datum->syntax #f the-string
                                   (list "TODO-need-port-name-here"
                                         line col pos (- end-pos pos))))
        (cond [(procedure? wrapper) (wrapper stx)]
              [(symbol? wrapper) (datum->syntax
                                  #f (list (datum->syntax
                                            #f wrapper
                                            (list "TODO-need-port-name-here"
                                                  line col pos (length l-delim)))
                                           stx))]
              [(not wrapper) stx]
              [else (error 'raw-string-parser "bad wrapper value: ~a" wrapper)])
        )
      #:end pos))))

(define (chido-readtable-add-raw-string-parser
         rt left right
         #:wrapper [wrapper #f]
         ;#:inside-readtable
         ;; TODO - what is the right name here?
         #:readtable-add-type [rt-add-type 'terminating]
         )
  (define right-parser
    (proc-parser #:name (format "trailing-right-delimiter_~a" right)
                 #:prefix right
                 (λ (port) (make-parse-failure
                            (format "Trailing right delimiter: ~a"
                                    right)))
                 #:promise-no-left-recursion? #t
                 #:use-port? #f))
  (extend-chido-readtable
   (extend-chido-readtable rt rt-add-type (make-raw-string-parser left right
                                                                  #:wrapper wrapper))
   rt-add-type right-parser))

(define current-readtable-read1-parser
  (proc-parser
   #:name "current-readtable-read1-parser"
   (λ (port) (parse* port (chido-readtable->read1
                           (current-chido-readtable))))))
(define current-readtable-layout*-parser
  (proc-parser
   #:name "current-readtable-layout*-parser"
   (λ (port) (parse* port (chido-readtable->layout*
                           (current-chido-readtable))))))
(define current-readtable-layout+-parser
  (proc-parser
   #:name "current-readtable-layout+-parser"
   (λ (port) (parse* port (chido-readtable->layout+
                           (current-chido-readtable))))))


(define (chido-readtable-add-mixfix-operator
         rt name-strsym
         #:layout [layout 'required]
         #:precidence-greater-than [precidence-greater-than '()]
         #:precidence-less-than [precidence-less-than '()]
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
                                            current-readtable-read1-parser
                                            s))
                                 name-split))
  (define parsers
    (match layout
      ['required (flatten
                  (cons (car parsers/no-layout)
                        (map (λ (p) (list current-readtable-layout+-parser p))
                             (cdr parsers/no-layout))))]
      ['optional (flatten
                  (cons (car parsers/no-layout)
                        (map (λ (p) (list current-readtable-layout*-parser p))
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
                 (λ (line col pos end-pos derivations)
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
                             result-indices))))
                 #:derivations derivations))
     parsers))

  (extend-chido-readtable rt 'left-recursive-nonterminating
                          #:precidence-greater-than precidence-greater-than
                          #:precidence-less-than precidence-less-than
                          #:associativity associativity
                          #:symbol-blacklist symbols-to-blacklist
                          #:operator operator-style
                          sequence-parser))

(define-syntax (chido-readtable-add-mixfix-operators stx)
  (syntax-parse stx
    [(_ start-readtable:expr
        [name:id (~or (~optional (~seq #:layout layout-arg:id))
                      (~optional (~seq #:precidence-greater-than [pgt:id ...]))
                      (~optional (~seq #:precidence-less-than [plt:id ...]))
                      (~optional (~seq #:associativity (~or assoc-arg:id #f))))
                 ...]
        ...)
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
                   rt
                   op-name
                   #:layout layout
                   #:associativity assoc
                   #:precidence-greater-than gts
                   #:precidence-less-than lts)
                  more)]))]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Testing
(module+ test
  (require rackunit)
  (require racket/stream)
  (define (make-<two>-parser)
    (proc-parser (λ (port)
                   (define s (read-string 5 port))
                   (define-values (line col pos) (port-next-location port))
                   (if (equal? s "<two>")
                       (make-parse-derivation s #:end pos)
                       (make-parse-failure "didn't match «<two>»")))))
  (define (make-<two/alt>-parser)
    (proc-parser (λ (port)
                   (define s (read-string 9 port))
                   (define-values (line col pos) (port-next-location port))
                   (if (equal? s "<two/alt>")
                       (stream-cons (make-parse-derivation s #:end pos)
                                    (stream-cons (make-parse-derivation s #:end pos)
                                                 empty-stream))
                       (make-parse-failure "didn't match «<two>»")))))

  (define my-rt
    (extend-chido-readtable*
     (chido-readtable-add-mixfix-operators
      (chido-readtable-add-mixfix-operator
       (chido-readtable-add-raw-string-parser
        (chido-readtable-add-raw-string-parser
         (chido-readtable-add-raw-string-parser
          (chido-readtable-add-list-parser
           (chido-readtable-add-list-parser
            (chido-readtable-add-list-parser empty-chido-readtable "(" ")")
            "[" "]")
           "$(" ")" #:wrapper '#%dollar-paren)
          "#|" "|#" #:readtable-add-type 'layout)
         "<<" ">>")
        "!!" "!!")
       "_<+>_" #:associativity 'left)
      [_<^>_ #:associativity right]
      [_<*>_ #:associativity left
             #:precidence-greater-than [<+>]
             #:precidence-less-than [<^>]]
      [<low-prefix>_ #:precidence-less-than [<+>]]
      [_<low-postfix> #:precidence-less-than [<+>]])
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
     'layout " "
     'layout "\n"
     'layout "\t"
     'layout (make-quote-parser "#;" 'quote)
     'layout (make-line-comment-parser ";")
     )
    )

  (define (p* string parser)
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
               (p* "   \t\n  " (chido-readtable-layout*-parser my-rt)))
   (check-equal? (p* "()" r1)
                 '(()))
   (check-equal? (p* "( )" r1)
                 '(()))
   (check-equal? (p* "( ( ))" r1)
                 '((())))
   (check-equal? (p* "( ( ) )" r1)
                 '((())))
   (check-equal? (p* "( ( ) )" r1)
                 '((())))
   (check-pred parse-failure? (p* ")" r1))
   (check-equal? (p* "testing" r1)
                 '(testing))
   (check-equal? (p* "(testing)" r1)
                 '((testing)))
   (check-equal? (p* "( testing)" r1)
                 '((testing)))
   (check-equal? (p* "(testing )" r1)
                 '((testing)))
   (check-equal? (p* "( testing )" r1)
                 '((testing)))
   (define s1 "(hello ( goodbye () ( ( ) ) ) bandicoot kangaroo ( aardvark   ))")
   (check-equal? (p* s1 r1)
                 (list (read (open-input-string s1))))
   (check-equal? (p* "\"testing 123\"" r1)
                 '("testing 123"))
   (check-equal? (p* "(this is \"a test\" of string reading)" r1)
                 '((this is "a test" of string reading)))

   (check-equal? (p* "(this (is \"a test\") of string reading)" r1)
                 '((this (is "a test") of string reading)))

   (check-equal? (p* "(this (is <<a test <<of raw>> string>>) reading)" r1)
                 '((this (is "a test <<of raw>> string") reading)))
   ;; If the left and right delimiters are the same, a raw string is not nestable.
   (check-equal? (p* "(this (is !!a test !!of raw!! string!!) reading)" r1)
                 '((this (is "a test " of raw " string") reading)))
   ;; this one had a weird error at one point.  I'm not sure what it was, but I'm leaving this here.
   (check-not-exn (λ () (p* "   \n   " (chido-readtable-layout*-parser my-rt))))

   (check-equal? (p* "(hello `(foo ,bar ,(#:testing #t #f #;(quoted #t))))" r1)
                 '((hello `(foo ,bar ,(#:testing #t #f)))))

   ;; TODO - I need to add a follow filter or something here...
   (check-equal? (p* "`(hello ,@foo)" r1)
                 '(`(hello ,@foo)))

   (check-equal? (p* "$(test foo (bar $(qwer)))" r1)
                 '((#%dollar-paren test foo (bar (#%dollar-paren qwer)))))
   (check-equal? (p* "[a b]" r1)
                 '[(a b)])

   (check-equal? (p* "(a <two> b)" r1)
                 '[(a "<two>" b) (a "<two>" b)])
   (check-equal? (p* "(a <two/alt> b)" r1)
                 '[(a "<two/alt>" b) (a "<two/alt>" b)])

   ;;; operators
   (check-equal? (p* "[() <+> ()]" r1)
                 '[((#%chido-readtable-infix-operator <+> () ()))])
   (check-equal? (p* "[() <+> () <+> ()]" r1)
                 '[((#%chido-readtable-infix-operator
                     <+> (#%chido-readtable-infix-operator <+> () ())
                     ()))])
   (check-equal? (p* "[() <^> () <^> ()]" r1)
                 '[((#%chido-readtable-infix-operator <^> () (#%chido-readtable-infix-operator <^> () ())))])
   (check-equal? (p* "[() <+> () <*> ()]" r1)
                 '[((#%chido-readtable-infix-operator <+> () (#%chido-readtable-infix-operator <*> () ())))])
   (check-equal? (p* "[() <*> () <+> ()]" r1)
                 '[((#%chido-readtable-infix-operator <+> (#%chido-readtable-infix-operator <*> () ()) ()))])
   (check-equal? (p* "[() <+> () <^> ()]" r1)
                 '[((#%chido-readtable-infix-operator <+> () (#%chido-readtable-infix-operator <^> () ())))])
   (check-equal? (p* "[() <^> () <+> ()]" r1)
                 '[((#%chido-readtable-infix-operator <+> (#%chido-readtable-infix-operator <^> () ()) ()))])


   (check-equal? (p* "[<low-prefix> a 1 2 3]" r1)
                 '[((#%chido-readtable-prefix-operator <low-prefix> a) 1 2 3)])
   (check-equal? (p* "[(testing 123) <+> (foo (bar))]" r1)
                 '[((#%chido-readtable-infix-operator <+> (testing 123) (foo (bar))))])

   (check-equal? (p* "[a <+> b 1 2 3]" r1)
                 '[((#%chido-readtable-infix-operator <+> a b) 1 2 3)])
   (check-equal? (p* "[a <low-postfix>]" r1)
                 '[((#%chido-readtable-postfix-operator <low-postfix> a))])
   (check-equal? (p* "[1 <+> 2 <+> 3 <+> 4]" r1)
                 '[((#%chido-readtable-infix-operator
                     <+>
                     (#%chido-readtable-infix-operator <+>
                                                       (#%chido-readtable-infix-operator <+> 1 2)
                                                       3)
                     4))])
   (check-equal? (p* "[1 <+> 2 <*> 3]" r1)
                 '[((#%chido-readtable-infix-operator <+> 1 (#%chido-readtable-infix-operator <*> 2 3)))])
   (check-equal? (p* "[1 <*> 2 <+> 3]" r1)
                 '[((#%chido-readtable-infix-operator <+> (#%chido-readtable-infix-operator <*> 1 2) 3))])

   ;;; operator deep precidence issue
   (check-equal? (p* "[#t <+> <low-prefix> #f <+> #t]" r1)
                 '[((#%chido-readtable-infix-operator
                     <+>
                     #t
                     (#%chido-readtable-prefix-operator
                      <low-prefix> (#%chido-readtable-infix-operator <+> #f #t))))])
   (check-equal? (p* "[#f <+> #t <low-postfix> <+> #f]" r1)
                 '[((#%chido-readtable-infix-operator
                     <+>
                     (#%chido-readtable-postfix-operator
                      <low-postfix> (#%chido-readtable-infix-operator <+> #f #t))
                     #f))])

   ;; check symbol blacklist that operators were added to
   (check-pred parse-failure?
               (p* "<+>" r1))

   (check-equal? (p* "some-symbol" r1)
                 '[some-symbol])
   (check-pred parse-failure?
               (p* "some-symbol" r1/no-symbol))


   #|
   TODO - operators
   * decide a better interface for adding operators
   ** maybe I should have an operator adding macro that does this conveniently -- eg. add mixfix, that allows holes for current-readtable and strings for the operator part names.  The whole parser name will be op_part_with_holes, leaving out pre and post underscores.  But each part of the mixfix operator will be added to the symbol blacklist.
   * Make some sort of whitespace-inserting sequence combinator.  Maybe there should be a more generic current-layout-parser that the readtable should use.  Or maybe it should be a readtable-specific combinator?

   * What about space requirements around operators, and whether they should be terminating, nonterminating, or soft-terminating?  Or whether the characters themselves should be disallowed in symbols?
   ** I think they should generally be nonterminating and have no requirements about the characters not being IN symbols.  In such restrictive languages maybe you just make a symbol parser and rely on this readtable implementation just for its operator handling, and turn off its automatic symbol parsing.

   * test infix operators
   * test prefix operators
   * test postfix operators
   * test deep precidence cases
   |#

   )

  )

(module+ an-s-exp-readtable
  (provide an-s-exp-readtable)
  (define an-s-exp-readtable
    (extend-chido-readtable*
     (chido-readtable-add-list-parser
      (chido-readtable-add-raw-string-parser
       (chido-readtable-add-raw-string-parser
        (chido-readtable-add-list-parser
         (chido-readtable-add-list-parser
          (chido-readtable-add-list-parser empty-chido-readtable "(" ")")
          "[" "]")
         "{" "}")
        "«" "»")
       "#|" "|#" #:readtable-add-type 'layout)
      "##{" "}##" #:readtable-add-type 'layout
      )
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
     'layout " "
     'layout "\n"
     'layout "\t"
     ;; creative use of string-append to get emacs to color properly...
     'layout (make-quote-parser (string-append "#" ";")'comment-quote)
     'layout (make-line-comment-parser ";")
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
