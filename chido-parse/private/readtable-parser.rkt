#lang racket/base

(require racket/contract/base)
(provide
 ;; TODO - the names provided should maybe be chido-readtable-* ...
 (contract-out
  ;; TODO - re-think what the empty readtable should be...
  [empty-chido-readtable chido-readtable?]
  [extend-chido-readtable (-> chido-readtable?
                              (or/c 'terminating 'soft-terminating
                                    'nonterminating 'layout)
                              any/c
                              chido-readtable?)]
  [chido-readtable-add-list-parser (-> chido-readtable?
                                       string? string?
                                       chido-readtable?)]
  [chido-readtable->read1 (-> chido-readtable? any/c)]
  [chido-readtable->read1/layout (-> chido-readtable? any/c)]
  [chido-readtable->read* (-> chido-readtable? any/c)]
  )
 extend-chido-readtable*
 current-chido-readtable

 ;; TODO - maybe not these?
 hash-t-parser
 hash-f-parser
 racket-style-string-parser

 )

(require
 "scheduler.rkt"
 "basic-combinators.rkt"
 "trie.rkt"
 "parameters.rkt"
 racket/match
 racket/list
 )


(struct chido-readtable
  (
   ;; Core parsers
   terminating-parsers
   soft-terminating-parsers
   nonterminating-parsers
   layout-parsers

   ;; Options
   symbol-result-transformer
   literal-left-delimiter
   literal-right-delimiter
   symbol-escape ;; IE backslash
   number-support?
   complex-number-support?

   ;; Basically these are cached results
   [flush-state? #:mutable]
   [terminating-trie #:mutable]
   [soft-terminating-trie #:mutable]
   [nonterminating-trie #:mutable]
   [layout-trie #:mutable]

   ;; These exist to ensure parsers created with chido-readtable are always eq?
   [layout*-parser #:mutable]
   [read1-parser #:mutable]
   [layout*+read1-parser #:mutable]
   [read*-parser #:mutable]
   )
 ; #:prop prop:custom-parser
 ; (λ (self)
 ;   ;; return a parser object
 ;   TODO)
  )


(define empty-chido-readtable
  (chido-readtable
   ;; core parsers
   '() '() '() '()
   ;; options
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
   ;;; cached stuff
   #t
   ;; tries
   empty-trie
   empty-trie
   empty-trie
   empty-trie
   ;; parsers
   #f
   #f
   #f
   #f
   ))

(define (extend-chido-readtable rt extension-type parser)
  ;; extension-type is 'terminating, 'soft-terminating, 'nonterminating, or 'layout
  (match extension-type
    ['terminating (struct-copy
                   chido-readtable
                   rt
                   [terminating-parsers
                    (cons parser (chido-readtable-terminating-parsers rt))]
                   [flush-state? #t])]
    ['soft-terminating (struct-copy
                        chido-readtable
                        rt
                        [soft-terminating-parsers
                         (cons parser (chido-readtable-soft-terminating-parsers rt))]
                        [flush-state? #t])]
    ['nonterminating (struct-copy
                      chido-readtable
                      rt
                      [nonterminating-parsers
                       (cons parser (chido-readtable-nonterminating-parsers rt))]
                      [flush-state? #t])]
    ['layout (struct-copy
              chido-readtable
              rt
              [layout-parsers
               (cons parser (chido-readtable-layout-parsers rt))]
              [flush-state? #t])]))

(define (extend-chido-readtable* rt . args)
  (match args
    [(list type parser rest-args ...)
     (apply extend-chido-readtable*
            (extend-chido-readtable rt type parser)
            rest-args)]
    [(list) rt]
    [else (error 'extend-chido-readtable* "bad number of arguments")]))

(define (parser-list->trie parsers)
  (for/fold ([t empty-trie])
            ([p parsers])
    (trie-add t (parser-prefix p) p)))

(define (chido-readtable-populate-cache! rt)
  (when (chido-readtable-flush-state? rt)
    (set-chido-readtable-terminating-trie!
     rt
     (parser-list->trie (chido-readtable-terminating-parsers rt)))
    (set-chido-readtable-soft-terminating-trie!
     rt
     (parser-list->trie (chido-readtable-soft-terminating-parsers rt)))
    (set-chido-readtable-nonterminating-trie!
     rt
     (parser-list->trie (chido-readtable-nonterminating-parsers rt)))
    (set-chido-readtable-layout-trie!
     rt
     (parser-list->trie (chido-readtable-layout-parsers rt)))
    (set-chido-readtable-layout*-parser!
     rt (kleene-star (make-alt-parser "chido-readtable-layout*"
                                      (chido-readtable-layout-parsers rt))
                     #:result (λ (elems) #f)))
    (set-chido-readtable-read1-parser!
     rt
     ;; TODO - better name!
     (proc-parser
      "chido-readtable-read1"
      ""
      (λ (port)
        (define parsers (append
                         (chido-readtable-nonterminating-parsers rt)
                         (chido-readtable-soft-terminating-parsers rt)
                         (chido-readtable-terminating-parsers rt)))
        (define alt (make-alt-parser "chido-readtable-read1/alt"
                                     parsers))
        (define parsers-result (parse* port alt))
        (if (parse-failure? parsers-result)
            (parse* port symbol/number-parser #:args (list rt))
            parsers-result))))
    (set-chido-readtable-layout*+read1-parser!
     rt
     (sequence (chido-readtable-layout*-parser rt)
               (chido-readtable-read1-parser rt)
               #:result (λ (l r) r)))
    (set-chido-readtable-read*-parser!
     rt
     (let ([with-content-parser
             (sequence (kleene-plus (chido-readtable-layout*+read1-parser rt))
                       (chido-readtable-layout*-parser rt)
                       #:result (λ (vals layout) vals))]
           [no-content-parser
            (sequence (chido-readtable-layout*-parser rt) #:result (λ (layout) '()))])
       ;; TODO - better name!
       (make-alt-parser "chido-readtable-read*"
                        (list with-content-parser no-content-parser))))
    (set-chido-readtable-flush-state?! rt #f)))


(define (parse-symbol/number-func port rt)
  ;; TODO - handle symbol escapes and literal delimiters

  (define-values (start-line start-col start-pos) (port-next-location port))
  ;; Trie-pairs are (cons trie n),
  ;; where n is the prefix length of trie that has matched so far.
  (define (rec/main len hard-trie-pairs soft-trie-pairs peek-offsets)
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
          (define soft-result (parse* port parser
                                      #:start (+ start-pos start-offset)))
          (if (not (parse-failure? soft-result))
              (cdr soft-pair)
              #f))))

    (define delimit-length (if (and soft-delimit-length hard-delimit-length)
                               (max soft-delimit-length hard-delimit-length)
                               (or soft-delimit-length hard-delimit-length)))


    (if delimit-length
        (- len delimit-length)
        (rec/step len new-hard-tries new-soft-tries peek-offsets)))

  (define (rec/step len hard-trie-pairs soft-trie-pairs peek-offsets)
    (define c (peek-char port (car peek-offsets)))
    (if (eof-object? c)
        len
        (let ([c-len (char-utf-8-length c)])
          (rec/main (add1 len)
                    (filter-map (step-trie-pair c) hard-trie-pairs)
                    (filter-map (step-trie-pair c) soft-trie-pairs)
                    (cons (+ c-len (car peek-offsets)) peek-offsets)))))
  (define ((step-trie-pair c) tp)
    (define next-trie (trie-step (car tp) c #f))
    (and next-trie (cons next-trie (add1 (cdr tp)))))

  ;; actual start of parsing symbols and numbers...

  (chido-readtable-populate-cache! rt)
  (define sym/num-length (rec/main 0 '() '() '(0)))
  (if (equal? 0 sym/num-length)
      (make-parse-failure "Can't parse symbol because delimiter succeeded parsing."
                          #:position start-pos)
      (let ()
        (define-values (line col pos) (port-next-location port))
        (define span sym/num-length)
        (define str (read-string sym/num-length port))
        ;; TODO - check options for whether complex numbers, rational numbers, and numbers of any kind are supported in this readtable...
        (define number (string->number str))
        (define datum (or number (string->symbol str)))
        (define stx (datum->syntax #f datum (list (object-name port)
                                                  line col pos span)))
        ;; TODO - use result transformer
        ;; TODO - by default this should be a syntax object, but for now I'll return a datum
        (make-parse-derivation datum #:end (+ pos span)))))

(define symbol/number-parser
  (proc-parser "symbol/number-parser" "" parse-symbol/number-func))


(define (chido-readtable->read1 rt)
  (chido-readtable-populate-cache! rt)
  (chido-readtable-read1-parser rt))
(define (chido-readtable->read* rt)
  (chido-readtable-populate-cache! rt)
  (chido-readtable-read*-parser rt))
(define (chido-readtable->read1/layout rt)
  (chido-readtable-populate-cache! rt)
  (chido-readtable-layout*+read1-parser rt))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Basic readtable-related parsers

;; TODO - what should the default be?
(define current-chido-readtable (chido-parse-parameter #f))


;; TODO - left and right must be strings
(define (chido-readtable-add-list-parser rt left right
                                         ;#:result-transformer
                                         ;#:inside-readtable
                                         )
  (define (inner-parser)
    (proc-parser (format "list-inner-parser-~a-~a" left right)
                 ""
                 (λ (port)
                   (define inner-rt (current-chido-readtable))
                   (parse* port (chido-readtable->read* inner-rt)))))
  (define left-parser
    (sequence left inner-parser right
              ;; TODO - use the optional transformer argument
              ;; TODO - the result should be a syntax object by default
              #:result (λ (l inner right) inner)))

  (define right-parser
    (proc-parser (format "trailing-right-delimiter_~a" right)
                 right
                 (λ (port) (make-parse-failure
                            (format "Trailing right delimiter: ~a"
                                    right)))))

  (extend-chido-readtable (extend-chido-readtable rt 'terminating left-parser)
                          'terminating right-parser))

#;(define (chido-readtable-add-raw-string-parser rt left right)
  TODO)

(define racket-style-string-parser
  (proc-parser "racket-style-string-parser"
               "\""
               (λ (port)
                 (define r (read port))
                 (define-values (line col pos) (port-next-location port))
                 (make-parse-derivation r #:end pos))))

(define hash-t-parser (result-modify "#t" (λ(x)#t)))
(define hash-f-parser (result-modify "#f" (λ(x)#f)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Testing
(module+ test
  (require rackunit)
  (require racket/stream)
  (define my-rt
    (extend-chido-readtable*
     (chido-readtable-add-list-parser empty-chido-readtable "(" ")")
     'terminating "##"
     'terminating racket-style-string-parser
     'nonterminating hash-t-parser
     'nonterminating hash-f-parser
     'layout " "
     'layout "\n"
     'layout "\t"))

  (define (p* string parser)
    (define r (parse* (open-input-string string) parser))
    (if (parse-failure? r)
        r
        (map parse-derivation-result
             (stream->list
              r))))

  (define r1 (chido-readtable->read1 my-rt))

  (chido-parse-parameterize
   ([current-chido-readtable my-rt])

   (check-equal? (p* "()" r1)
                 '(()))
   (check-equal? (p* "( )" r1)
                 '(()))
   (check-equal? (p* "( ( ) )" r1)
                 '((())))
   (check-pred parse-failure? (p* ")" r1))
   (define s1 "(hello ( goodbye () ( ( ) ) ) aoeu aoeu ( aardvark   ))")
   (check-equal? (p* s1 r1)
                 (list (read (open-input-string s1))))
   (check-equal? (p* "\"testing 123\"" r1)
                 '("testing 123"))
   (check-equal? (p* "(this is \"a test\" of string reading)" r1)
                 '((this is "a test" of string reading)))

   (check-equal? (p* "(this (is \"a test\") of string reading)" r1)
                 '((this (is "a test") of string reading)))

   ;; TODO - this one has an interesting error that I want to get to later...
   ;(p* "   \n   " (chido-readtable-layout*-parser my-rt))

   ;; TODO - this is not returning the right failure object.
   ;(p* "hello" "arg")

   )

  )