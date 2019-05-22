#lang racket/base

(provide
 ;; TODO - the names provided should maybe be chido-readtable-* ...
 (contract-out
  ;; TODO - re-think what the empty readtable should be...
  [empty-readtable readtable?]
  [extend-readtable (-> readtable?
                        (or/c 'terminating 'soft-terminating
                              'nonterminating 'layout)
                        any/c
                        readtable?)]
  [readtable->read1 (-> readtable? any/c)]
  [readtable->read1/layout (-> readtable? any/c)]
  [readtable->read* (-> readtable? any/c)]
  )

 )

(require
 "scheduler.rkt"
 "basic-combinators.rkt"
 "trie.rkt"
 )


(struct readtable
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

   ;; These exist to ensure parsers created with readtable are always eq?
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


(define empty-readtable
  (readtable
   ;; core parsers
   '() '() '() '()
   ;; options
   #f
   ;; TODO - should literal delimiters and backslash be set on the empty readtable, or should they be #f?
   ;; TODO - also, when literal delimiters are NOT equal, should they be nestable?  I think yes.  Maybe there should be an option?
   "|" "|" "\\"
   #t #t
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

(define (extend-readtable rt extension-type parser)
  ;; extension-type is 'terminating, 'soft-terminating, 'nonterminating, or 'layout
  (match extension-type
    ['terminating (struct-copy
                   readtable
                   rt
                   [terminating-parsers
                    (cons parser (readtable-terminating-parsers rt))]
                   [flush-state? #t])]
    ['soft-terminating (struct-copy
                        readtable
                        rt
                        [soft-terminating-parsers
                         (cons parser (readtable-soft-terminating-parsers rt))]
                        [flush-state? #t])]
    ['nonterminating (struct-copy
                      readtable
                      rt
                      [nonterminating-parsers
                       (cons parser (readtable-nonterminating-parsers rt))]
                      [flush-state? #t])]
    ['layout (struct-copy
              readtable
              rt
              [layout-parsers
               (cons parser (readtable-layout-parsers rt))]
              [flush-state? #t])]))

(define (parser-list->trie parsers)
  (for/fold ([t empty-trie])
            ([p parsers])
    (trie-add t (parser-prefix p) p)))

(define (readtable-populate-cache! rt)
  (when (readtable-flush-state? rt)
    (set-readtable-terminating-trie!
     rt
     (parser-list->trie (readtable-terminating-parsers rt)))
    (set-readtable-soft-terminating-trie!
     rt
     (parser-list->trie (readtable-soft-terminating-parsers rt)))
    (set-readtable-nonterminating-trie!
     rt
     (parser-list->trie (readtable-nonterminating-parsers rt)))
    (set-readtable-layout-trie!
     rt
     (parser-list->trie (readtable-layout-parsers rt)))
    (set-readtable-layout*-parser!
     rt (kleene-star (make-alt-parser "readtable-layout*"
                                      (readtable-layout-parsers rt))
                     #:result (λ (elems) #f)))
    (set-readtable-read1-parser!
     rt
     ;; TODO - better name!
     (proc-parser
      "readtable-read1"
      ""
      (λ (port)
        (define parsers (append
                         (readtable-nonterminating-parsers rt)
                         (readtable-soft-terminating-parsers rt)
                         (readtable-terminating-parsers rt)))
        (define alt (make-alt-parser "readtable-read1/alt"
                                     parsers))
        (define parsers-result (parse* port alt))
        (if (parse-failure? parsers-result)
            (parse* port parse-symbol/number rt)
            parsers-result))))
    (set-readtable-layout*+read1-parser!
     rt
     (sequence (readtable-layout*-parser rt)
               (readtable-read1-parser rt)
               #:result (λ (l r) r)))
    (set-readtable-read*-parser!
     rt
     (let ([with-content-parser
             (sequence (kleene-plus (sequence (readtable-layout*-parser rt)
                                              (readtable-read1-parser rt)
                                              #:result (λ (layout val) val)))
                       (readtable-layout*-parser rt)
                       #:result (λ (vals layout) vals))]
           [no-content-parser
            (sequence (readtable-layout*-parser rt) #:result (λ (layout) '()))])
       ;; TODO - better name!
       (make-alt-parser "readtable-read*"
                        (list with-content-parser no-content-parser))))
    (set-readtable-flush-state?! #f)))


(define (parse-symbol/number port rt)
  ;; TODO - handle symbol escapes and literal delimiters

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
          (define start-length (- len (cdr soft-pair)))
          (if (not (parse-failure? (parse* port parser TODO-starting-location)))
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
                    (filter-map step-trie-pair hard-trie-pairs)
                    (filter-map step-trie-pair soft-trie-pairs)
                    (cons (+ c-len (car peek-offsets)) peek-offsets)))))
  (define (step-trie-pair tp)
    (define next-trie (trie-step (car tp) #f))
    (and next-trie (cons next-trie (add1 (cdr tp)))))

  ;; actual start of parsing symbols and numbers...

  (readtable-populate-cache! rt)
  (define sym/num-length (rec/main 0 '() '() '(0)))
  (if (equal? 0 sym/num-length)
      TODO-parse-failure
      (let ()
        (define-values (line col pos) (port-next-location port))
        (define span sym/num-length)
        (define str (read-string sym/num/length port))
        ;; TODO - check options for whether complex numbers, rational numbers, and numbers of any kind are supported in this readtable...
        (define number (string->number str))
        (define datum (or number str))
        (define stx (datum->syntax #f datum (list (object-name port)
                                                  line col pos span)))
        ;; TODO - use result transformer
        (make-parse-derivation stx #:end (+ pos span)))))


(define (readtable->read1 rt)
  (readtable-populate-cache! rt)
  (readtable-read1-parser rt))
(define (readtable->read* rt)
  (readtable-populate-cache! rt)
  (readtable-read*-parser rt))
(define (readtable->read1/layout rt)
  (readtable-populate-cache! rt)
  (readtable-layout*+read1-parser rt))


