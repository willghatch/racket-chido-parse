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
 "procedural-combinators.rkt"
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
   [symbol-parser #:mutable]
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
    (set-chido-readtable-symbol-parser! rt (symbol/number-parser rt))
    (set-chido-readtable-layout-trie!
     rt
     (parser-list->trie (chido-readtable-layout-parsers rt)))
    (set-chido-readtable-layout*-parser!
     rt (kleene-star (make-alt-parser "chido-readtable-layout"
                                      (chido-readtable-layout-parsers rt))
                     #:derive (λ (elems) (make-parse-derivation
                                          '() #:derivations elems))
                     #:greedy? #t))
    (set-chido-readtable-read1-parser!
     rt
     ;; TODO - better name!
     (proc-parser
      #:name "chido-readtable-read1"
      ""
      (let* ([parsers (append
                       (chido-readtable-nonterminating-parsers rt)
                       (chido-readtable-soft-terminating-parsers rt)
                       (chido-readtable-terminating-parsers rt))]
             [alt (make-alt-parser "chido-readtable-read1/alt"
                                   parsers)])
        (λ (port)
          (chido-parse-parameterize
           ([current-chido-readtable rt])
           (define parsers-result (parse* port alt))
           (if (parse-failure? parsers-result)
               (parse* port (chido-readtable-symbol-parser rt))
               parsers-result))))
      #:use-port? #f
      #:promise-no-left-recursion?
      (not (ormap parser-potentially-left-recursive?
                  (append (chido-readtable-nonterminating-parsers rt)
                          (chido-readtable-soft-terminating-parsers rt)
                          (chido-readtable-terminating-parsers rt)
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
    (set-chido-readtable-read*-parser!
     rt
     (let ([with-content-parser
             (sequence
              (kleene-plus (chido-readtable-layout*+read1-parser rt) #:greedy? #t)
              (chido-readtable-layout*-parser rt)
              #:derive (λ derivations
                         (make-parse-derivation
                          (λ (line col pos end-pos derivations)
                            (parse-derivation-result (first derivations)))
                          #:derivations derivations)))]
           [no-content-parser (chido-readtable-layout*-parser rt)])
       ;; TODO - better name!
       (make-alt-parser "chido-readtable-read*"
                        (list with-content-parser no-content-parser))))
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
  (if (equal? 0 sym/num-length)
      (make-parse-failure "Can't parse symbol because delimiter succeeded parsing."
                          #:position start-pos)
      (let ()
        (define span sym/num-length)
        (define str (port-broker-substring pb start-pos sym/num-length))
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
        (make-parse-derivation result-func #:end (+ start-pos span)))))

(define (symbol/number-parser rt)
  (proc-parser #:name "symbol/number-parser" "" (parse-symbol/number-func rt)
               #:promise-no-left-recursion? #t
               #:use-port? #f))


(define (chido-readtable->read1 rt)
  (chido-readtable-populate-cache! rt)
  (chido-readtable-read1-parser rt))
(define (chido-readtable->read* rt)
  (chido-readtable-populate-cache! rt)
  (chido-readtable-read*-parser rt))
(define (chido-readtable->read1/layout rt)
  (chido-readtable-populate-cache! rt)
  (chido-readtable-layout*+read1-parser rt))
(define (chido-readtable->symbol rt)
  (chido-readtable-populate-cache! rt)
  (chido-readtable-symbol-parser rt))
(define (chido-readtable->layout* rt)
  (chido-readtable-populate-cache! rt)
  (chido-readtable-layout*-parser rt))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Basic readtable-related parsers

;; TODO - what should the default be?
(define current-chido-readtable (chido-parse-parameter #f))


;; TODO - left and right must be strings
(define (chido-readtable-add-list-parser rt left right
                                         #:wrapper [wrapper #f]
                                         ;#:inside-readtable
                                         )
  (define (inner-parser)
    (proc-parser #:name (format "list-inner-parser-~a-~a" left right)
                 ""
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
                                                      line col pos (length left)))
                                               pre-result))]
                     [#f (->stx pre-result)]))
                 #:derivations derivations))
     ;#:result (λ (l inner right) inner)
     ))

  (define right-parser
    (proc-parser #:name (format "trailing-right-delimiter_~a" right)
                 right
                 (λ (port) (make-parse-failure
                            (format "Trailing right delimiter: ~a"
                                    right)))
                 #:promise-no-left-recursion? #t
                 #:use-port? #f))

  (extend-chido-readtable (extend-chido-readtable rt 'terminating left-parser)
                          'terminating right-parser))

#;(define (chido-readtable-add-raw-string-parser rt left right)
  TODO)

(define racket-style-string-parser
  (proc-parser #:name "racket-style-string-parser"
               "\""
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
   ""
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
   prefix
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
             ""
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

(define (make-raw-string-parser l-delim r-delim)
  (proc-parser
   #:name "raw-string"
   l-delim
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
        (datum->syntax #f the-string
                       (list "TODO-need-port-name-here"
                             line col pos (- end-pos pos))))
      #:end pos))))

(define current-readtable-read1-parser
  (proc-parser
   ""
   (λ (port) (parse* port (chido-readtable->read1
                           (current-chido-readtable))))))
(define current-readtable-layout*-parser
  (proc-parser
   ""
   (λ (port) (parse* port (chido-readtable->layout*
                           (current-chido-readtable))))))

(define (make-readtable-infix-operator op-string)
  (sequence
   current-readtable-read1-parser
   current-readtable-layout*-parser
   op-string
   current-readtable-layout*-parser
   current-readtable-read1-parser
   #:derive (λ derivations
              (make-parse-derivation
               (λ (line col pos end-pos derivations)
                 (datum->syntax
                  #f
                  `(#%readtable-infix ,(string->symbol op-string)
                                      ,(parse-derivation-result (first derivations))
                                      ,(parse-derivation-result (fifth derivations)))))
               #:derivations derivations))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Testing
(module+ test
  (require rackunit)
  (require racket/stream)
  (define my-rt
    (extend-chido-readtable*
     (chido-readtable-add-list-parser
      (chido-readtable-add-list-parser
       (chido-readtable-add-list-parser empty-chido-readtable "(" ")")
       "[" "]")
      "$(" ")" #:wrapper '#%dollar-paren)
     'terminating "##"
     'terminating racket-style-string-parser
     'terminating (make-raw-string-parser "<<" ">>")
     'terminating (make-raw-string-parser "!!" "!!")
     'nonterminating hash-t-parser
     'nonterminating hash-f-parser
     'terminating (make-quote-parser "'" 'quote)
     'terminating (make-quote-parser "`" 'quasiquote)
     'terminating (make-quote-parser "," 'unquote)
     'terminating (make-quote-parser ",@" 'unquote-splicing)
     'terminating (make-quote-parser "#'" 'syntax)
     'terminating (make-quote-parser "#`" 'quasisyntax)
     'terminating (make-quote-parser "#," 'unsyntax)
     'terminating (make-quote-parser "#,@" 'unsyntax-splicing)
     'terminating (make-keyword-parser "#:")
     'nonterminating (make-readtable-infix-operator "<+>")
     'layout " "
     'layout "\n"
     'layout "\t"
     'layout (make-quote-parser "#;" 'quote)
     'layout (make-line-comment-parser ";")
     ))

  (define (p* string parser)
    (define r (with-handlers ([(λ(e)#t)(λ(e)e)])
                (parse* (open-input-string string) parser)))
    (if (or (parse-failure? r)
            (exn? r))
        r
        (map parse-derivation-result
             (stream->list
              r))))

  (define r1 (chido-readtable->read1 my-rt))

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
   (define s1 "(hello ( goodbye () ( ( ) ) ) aoeu aoeu ( aardvark   ))")
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
   ;; TODO - this one has an interesting error that I want to get to later...
   ;(p* "   \n   " (chido-readtable-layout*-parser my-rt))

   (check-equal? (p* "(hello `(foo ,bar ,(#:testing #t #f #;(quoted #t))))" r1)
                 '((hello `(foo ,bar ,(#:testing #t #f)))))

   ;; TODO - I need to add a follow filter or something here...
   #;(check-equal? (p* "`(hello ,@foo)" r1)
                 '(`(hello ,@foo)))

   (check-equal? (p* "$(test foo (bar $(qwer)))" r1)
                 '((#%dollar-paren test foo (bar (#%dollar-paren qwer)))))

   ;; TODO - I need to add some kind of filter here too...  Probably if I want to support this sort of thing I need to simultaneously add the infix operator AND add a failure parser for that specific symbol, such that it fails as a symbol.
   ;; TODO - This kind of infix operator is a bad idea for multiple reasons, I think, beyond it adding ambiguity to read1
   #;(check-equal? (p* "[(testing 123) <+> (foo (bar))]" r1)
                 '[(#%readtable-infix <+> (testing 123) (foo (bar)))])

   )

  )

(module+ an-s-exp-readtable
  (provide an-s-exp-readtable)
  (define an-s-exp-readtable
    (extend-chido-readtable*
     (chido-readtable-add-list-parser
      (chido-readtable-add-list-parser
       (chido-readtable-add-list-parser empty-chido-readtable "(" ")")
       "[" "]")
      "{" "}")
     'nonterminating hash-t-parser
     'nonterminating hash-f-parser
     'terminating racket-style-string-parser
     'terminating (make-quote-parser "'" 'quote)
     'terminating (make-quote-parser "`" 'quasiquote)
     'terminating (make-quote-parser "," 'unquote)
     'terminating (make-quote-parser ",@" 'unquote-splicing)
     'terminating (make-quote-parser "#'" 'syntax)
     'terminating (make-quote-parser "#`" 'quasisyntax)
     'terminating (make-quote-parser "#," 'unsyntax)
     'terminating (make-quote-parser "#,@" 'unsyntax-splicing)
     'terminating (make-keyword-parser "#:")
     'layout " "
     'layout "\n"
     'layout "\t"
     'layout (make-quote-parser "#;" 'quote)
     'layout (make-line-comment-parser ";")
     'layout (make-raw-string-parser "#|" "|#")
     ))
  )
