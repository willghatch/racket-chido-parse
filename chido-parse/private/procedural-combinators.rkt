#lang racket/base

(provide
 sequence
 repetition
 kleene-star
 kleene-plus
 kleene-question
 epsilon-parser
 eof-parser
 not-parser
 peek-parser


 ;; TODO - These are not great, should probably be replaced
 traditional-read-func->parse-result-func
 wrap-derivation
 as-syntax

 regexp->parser

 ;; filters
 parse-filter
 follow-filter
 derivation-filter
 result-filter

 char-parser
 char-range-parser
 any-char-parser

 whole-parse*
 whole-parse
 parse
 )

(require
 "core-use.rkt"
 "parse-stream.rkt"
 racket/string
 racket/stream
 racket/match
 racket/list
 (for-syntax
  racket/base
  syntax/parse
  ))

(module+ test
  (require
   rackunit
   "test-util-2.rkt"
   ))

(define-syntax (/end stx)
  ;; quick hack to get end positions working with empty derivation lists
  (syntax-parse stx
    [(_ n body)
     #'(parameterize ([current-chido-parse-derivation-implicit-end n])
         body)]))


(define (sequence #:name [name #f]
                  #:derive [derive #f]
                  #:result/bare [make-result/bare #f]
                  #:result/stx [make-result/stx #f]
                  #:between [between #f]
                  #:before [before #f]
                  #:after [after #f]
                  . parsers)
  (define before-parser (match before
                          [#f #f]
                          [#t between]
                          [else before]))
  (define after-parser (match after
                         [#f #f]
                         [#t between]
                         [else after]))
  (define (l-recursive? parsers)
    ;; TODO - I should disallow a null list of parsers...
    (cond [(null? parsers) #f]
          [(parser-potentially-left-recursive? (car parsers)) #t]
          [(parser-potentially-null? (car parsers)) (l-recursive? (cdr parsers))]
          [else #f]))
  (define parsers-for-left-recursion-check
    (filter (λ(x)x)
            (match parsers
              [(list p1 ps ...)
               (flatten (list before-parser p1 between ps after-parser))]
              [(list) (list before-parser after-parser)])))
  (define l-r (l-recursive? parsers-for-left-recursion-check))
  (define prefix (parser-prefix (car parsers-for-left-recursion-check)))

  (define use-name (or name (format "sequence_~a"
                                    (string-join (map parser-name parsers) "_"))))
  (define (make-result-wrap make-result stx?)
    (λ derivations
      (make-parse-derivation
       (λ (src line col pos span derivations)
         (define non-between-ds
           (derivations->non-between derivations before-parser between after-parser))
         (let ([pre-stx (apply make-result
                               (map parse-derivation-result non-between-ds))])
           (if stx?
               (datum->syntax #f pre-stx
                              (list src line col pos span))
               pre-stx)))
       #:derivations derivations)))
  (define combiner
    (cond [(> 2 (count not (list derive make-result/bare make-result/stx)))
           (error 'sequence
                  "must provide either result or derive function, not both")]
          [derive derive]
          [(eq? #t make-result/stx) (make-result-wrap list #t)]
          [make-result/stx (make-result-wrap make-result/stx #t)]
          [(eq? #t make-result/bare) (make-result-wrap list #f)]
          [make-result/bare (make-result-wrap make-result/bare #f)]
          [else (make-result-wrap list #t)]))
  (define (proc pb)
    (define (rec parsers derivations in-between?)
      (cond [(null? parsers)
             (if after-parser
                 (for/parse ([d (parse* pb after-parser
                                        #:start (if (null? derivations)
                                                    #f
                                                    (car derivations)))])
                            (/end (port-broker-start-position pb)
                                  (apply combiner (reverse (cons d derivations)))))
                 (/end (port-broker-start-position pb)
                       (apply combiner (reverse derivations))))]
            [else (for/parse ([d (parse* pb (if in-between?
                                                between
                                                (car parsers))
                                         #:start (if (null? derivations)
                                                     #f
                                                     (car derivations)))])
                             (rec (if in-between? parsers (cdr parsers))
                                  (cons d derivations)
                                  (and between (not in-between?))))]))
    (if before-parser
        (for/parse ([d (parse* pb before-parser #:start #f)])
                   (rec parsers (list d) #f))
        (rec parsers '() #f)))
  (proc-parser #:name use-name #:prefix prefix proc
               #:promise-no-left-recursion? (not l-r)
               #:preserve-prefix? #t
               #:use-port? #f))


(define (derivations->non-between derivations before between after)
  (define (every-other xs)
    (match xs
      [(list) (list)]
      [(list a b others ...)
       (cons b (every-other others))]
      ;; if there is just one, it is the after-parser derivation.
      [(list a) (list)]))
  (cond [between (every-other (if before
                                  derivations
                                  (cons 'dummy
                                        derivations)))]
        [(and before after)
         (reverse (cdr (reverse (cdr derivations))))]
        [before (cdr derivations)]
        [after (reverse (cdr (reverse derivations)))]
        [else derivations]))


(define (repetition #:name [name #f]
                    #:derive [derive #f]
                    #:result/bare [make-result/bare #f]
                    #:result/stx [make-result/stx #f]
                    #:min [min 0]
                    #:max [max +inf.0]
                    #:greedy? [greedy? #t]
                    #:between [between #f]
                    #:before [before #f]
                    #:after [after #f]
                    parser)
  (define use-name (or name (format "repeat_~a_~a_~a"
                                    (parser-name parser)
                                    min max)))
  (define before-parser (match before
                          [#f #f]
                          [#t between]
                          [else before]))
  (define after-parser (match after
                         [#f #f]
                         [#t between]
                         [else after]))

  (define make-result-wrap
    (λ (make-result stx?)
      (λ (derivations)
        (make-parse-derivation
         (λ (src line col pos span derivations)
           (define non-between-ds
             (derivations->non-between derivations before-parser between after-parser))
           (let ([pre-stx (make-result (map parse-derivation-result non-between-ds))])
             (if stx?
                 (datum->syntax #f pre-stx (list src line col pos span))
                 pre-stx)))
         #:derivations derivations))))

  (define combiner
    (cond [(> 2 (count not (list derive make-result/bare make-result/stx)))
           (error 'repetition
                  "must provide either result or derive function, (and not both)")]
          [derive derive]
          [(eq? #t make-result/stx) (make-result-wrap (λ(x)x) #t)]
          [make-result/stx (make-result-wrap make-result/stx #t)]
          [(eq? #t make-result/bare) (make-result-wrap (λ(x)x) #f)]
          [make-result/bare (make-result-wrap make-result/bare #f)]
          [else (make-result-wrap (λ(x)x) #t)]))
  (define (proc pb)
    (define (finalize reversed-derivations end-failure)
      (define (real-finalize! reversed-derivations)
        (let ([derivation (/end (port-broker-start-position pb)
                                (combiner (reverse reversed-derivations)))])
          (if end-failure
              (parse-stream-cons derivation end-failure)
              derivation)))
      (if after-parser
          (let ([after-stream (parse* pb after-parser
                                      #:start (if (null? reversed-derivations)
                                                  #f
                                                  (car reversed-derivations)))])
            (for/parse ([d after-stream])
                       (real-finalize! (cons d reversed-derivations))))
          (real-finalize! reversed-derivations)))
    (define (get-more-streams n-results derivations in-between?)
      (define use-parser (cond [(and in-between? (equal? 0 n-results))
                                before-parser]
                               [in-between? between]
                               [else parser]))
      (define next-stream
        (parse* pb use-parser #:start (if (null? derivations)
                                          #f
                                          (car derivations))))
      (if (and greedy?
               (stream-empty? next-stream)
               (<= min n-results))
          (finalize derivations next-stream)
          (for/parse ([derivation next-stream])
                     (if in-between?
                         (get-more-streams n-results
                                           (cons derivation derivations)
                                           #f)
                         (rec (add1 n-results)
                              (cons derivation derivations))))))
    (define (rec n-results derivations)
      (define len n-results)
      (define do-between? (and between (not (equal? 0 n-results))))
      (cond [(or (< len min)
                 (and greedy? (< len max)))
             (get-more-streams n-results derivations do-between?)]
            [else (parse-stream-cons (finalize derivations #f)
                                     (if (>= len max)
                                         empty-stream
                                         (get-more-streams n-results
                                                           derivations
                                                           do-between?)))]))
    (if before-parser
        (for/parse ([d (parse* pb before-parser #:start #f)])
                   (rec 0 (list d)))
        (rec 0 '())))
  (proc-parser #:name use-name proc
               #:promise-no-left-recursion?
               (not (parser-potentially-left-recursive? parser))
               #:use-port? #f))

(define (kleene-star #:name [name #f]
                     #:derive [derive #f]
                     #:result/bare [make-result/bare #f]
                     #:result/stx [make-result/stx #f]
                     #:greedy? [greedy? #t]
                     #:between [between #f]
                     #:before [before #f]
                     #:after [after #f]
                     parser)
  (repetition parser
              #:name (or name (format "~a*" (parser-name parser)))
              #:derive derive
              #:result/bare make-result/bare
              #:result/stx make-result/stx
              #:greedy? greedy?
              #:between between
              #:before before
              #:after after
              #:min 0))

(define (kleene-plus #:name [name #f]
                     #:derive [derive #f]
                     #:result/bare [make-result/bare #f]
                     #:result/stx [make-result/stx #f]
                     #:greedy? [greedy? #t]
                     #:between [between #f]
                     #:before [before #f]
                     #:after [after #f]
                     parser)
  (repetition parser
              #:name (or name (format "~a+" (parser-name parser)))
              #:derive derive
              #:result/bare make-result/bare
              #:result/stx make-result/stx
              #:greedy? greedy?
              #:between between
              #:before before
              #:after after
              #:min 1))

(define (kleene-question #:name [name #f]
                         #:derive [derive #f]
                         #:result/bare [make-result/bare #f]
                         #:result/stx [make-result/stx #f]
                         #:greedy? [greedy? #t]
                         #:between [between #f]
                         #:before [before #f]
                         #:after [after #f]
                         parser)
  (repetition parser
              #:name (or name (format "~a?" (parser-name parser)))
              #:derive derive
              #:result/bare make-result/bare
              #:result/stx make-result/stx
              #:greedy? greedy?
              #:between between
              #:before before
              #:after after
              #:min 0
              #:max 1))

(define (optional #:name [name #f]
                  #:derive [derive #f]
                  #:result/bare [make-result/bare #f]
                  #:result/stx [make-result/stx #f]
                  parser)
  (repetition parser
              #:name (or name (format "~a?" (parser-name parser)))
              #:derive derive
              #:result/bare make-result/bare
              #:result/stx make-result/stx
              #:max 1))


(define (epsilon-parser #:name [name "epsilon"]
                        #:result [result #f])
  (proc-parser #:name name
               (λ (p)
                 (make-parse-derivation result
                                        #:end (port-broker-start-position p)))
               #:promise-no-left-recursion? #t
               #:use-port? #f))

(define eof-parser
  (proc-parser #:name "eof"
               (λ (pb)
                 (define pos (port-broker-start-position pb))
                 (define c (port-broker-char pb pos))
                 ;(define c (peek-char p))
                 ;(define-values (line col pos) (port-next-location p))
                 (if (eof-object? c)
                     (make-parse-derivation c #:end pos)
                     (make-parse-failure #:message "not eof" #:position pos)))
               #:promise-no-left-recursion? #t
               #:use-port? #f))

(define (not-parser parser
                    #:name [name (format "not_~a" (parser-name parser))]
                    #:result [result #f])
  (proc-parser #:name name
               (λ (p)
                 (define inner-result (parse* p parser))
                 (define-values (line col pos) (port-next-location p))
                 (if (parse-failure? inner-result)
                     (make-parse-derivation result #:end pos)
                     (make-parse-failure #:message "succeeded parsing in not parser"
                                         #:position pos)))))

(define (peek-parser parser)
  (proc-parser
   (λ (p)
     (define start (port->pos p))
     (define inner-result (parse* p parser))
     (for/parse ([d inner-result])
                (make-parse-derivation
                 #:end start #:derivations d
                 (λ (src line col pos span derivations)
                   (parse-derivation-result d)))))))



(define (wrap-derivation parser wrap-func #:name [name #f])
  (proc-parser #:name (or name (parser-name parser))
               #:prefix (parser-prefix parser)
               (λ (port)
                 (define s (parse* port parser))
                 (if (parse-failure? s)
                     s
                     (stream-map (λ (d) (make-parse-derivation
                                         (wrap-func d)
                                         #:derivations (list d) ))
                                 s)))
               #:preserve-prefix? #t
               #:promise-no-left-recursion?
               (not (parser-potentially-left-recursive? parser))
               #:use-port? #f))

(define (as-syntax parser)
  (wrap-derivation parser
                   (λ (d)
                     (λ (src line col pos span derivations)
                       (datum->syntax #f
                                      (parse-derivation-result d)
                                      (list src line col pos span))))))

(define (traditional-read-func->parse-result-func f #:syntax? [syntax? #f])
  (λ (port)
    (define result (if syntax?
                       (f (object-name port) port)
                       (f port)))
    (define-values (line col pos) (port-next-location port))
    (make-parse-derivation result
                           #:end pos
                           #:derivations '())))



(define (regexp->parser rx #:name [name #f])
  (define (parse-regexp port r failure-message)
    (define-values (line col pos) (port-next-location port))
    (define m (regexp-match r port))
    (define-values (end-line end-col end-pos) (port-next-location port))
    (if m
        (make-parse-derivation m #:end end-pos)
        (make-parse-failure #:message failure-message #:position pos)))
  (define failure-message (format "Didn't match regexp: ~a" (or name rx)))
  (proc-parser #:name (or name (format "~a" rx))
               (λ (p) (parse-regexp p rx failure-message))
               #:promise-no-left-recursion? #t))

(module+ test
  (define-syntax (c stx)
    (syntax-parse stx
      [(_ check-name arg ...)
       (datum->syntax stx
                      (syntax-e #'(check-not-exn (λ () (check-name arg ...))))
                      stx)]))

  (define ap "a")
  (define bp "b")
  (define cp "c")
  (define (Bp) (make-alt-parser "B"
                                (list bp
                                      (sequence #:name "Bb"
                                                #:result/bare string-append
                                                Bp bp))))
  (define aBcp (sequence #:name "aBc"
                         #:result/bare string-append
                         ap Bp cp))
  (define BaBcp (sequence #:name "aBc"
                          #:result/bare string-append
                          Bp ap Bp cp))


  (define str1 "abbbbbbc")
  (define p1 (open-input-string str1))
  (define r1 (parse* p1 aBcp))
  (c check-equal?
     (parse-derivation-result (car (stream->list r1)))
     str1)

  (define str2 "bbbabbbbbbbbbbbc")
  (c check-equal?
     (p*/r str2 BaBcp)
     (list str2))

  (c check-equal?
     (p*/r "qqq"
           (kleene-star "q"
                        #:result/bare (λ (elems) (string-join elems ""))
                        #:greedy? #f))
     (list "" "q" "qq" "qqq"))
  (c check-equal?
     (p*/r "qqq"
           (kleene-plus "q"
                        #:result/bare (λ (elems) (string-join elems ""))
                        #:greedy? #f))
     (list "q" "qq" "qqq"))
  (c check-equal?
     (p*/r "q"
           (kleene-question "q"
                            #:result/bare (λ (elems) (string-join elems ""))
                            #:greedy? #f))
     (list "" "q"))
  (c check-equal?
     (p*/r "qqqqqqqqqqqqqqqqqqqqqqq"
           (repetition "q" #:result/bare (λ (elems) (string-join elems ""))
                       #:min 3 #:max 5 #:greedy? #f))
     (list "qqq" "qqqq" "qqqqq"))

  ;;; repetition with between parsers
  (c check-equal?
     (p*/r "qaqaq"
           (repetition "q"
                       #:result/bare (λ (elems) (string-join elems ""))
                       #:between "a"
                       #:greedy? #f))
     (list "" "q" "qq" "qqq"))
  (c check-equal?
     (p*/r "bqaqaq"
           (repetition "q" #:result/bare (λ (elems) (string-join elems ""))
                       #:before "b"
                       #:between "a"
                       #:greedy? #f))
     (list "" "q" "qq" "qqq"))
  (c check-equal?
     (p*/r "bqaqaq"
           (repetition "q" #:result/bare (λ (elems) (string-join elems ""))
                       #:before "b"
                       #:between "a"
                       #:greedy? #f))
     (list "" "q" "qq" "qqq"))
  (c check-equal?
     (p*/r "bqaqaqz"
           (repetition "q" #:result/bare (λ (elems) (string-join elems ""))
                       #:before "b"
                       #:after "z"
                       #:between "a"
                       #:greedy? #f))
     (list "qqq"))
  (c check-equal?
     (p*/r "aqaqaqa"
           (repetition "q" #:result/bare (λ (elems) (string-join elems ""))
                       #:before #t
                       #:after #t
                       #:between "a"
                       #:greedy? #f))
     ;; no empty string, because it must parse before AND after...
     (list "q" "qq" "qqq"))
  (c check-equal?
     (p*/r "bqqq"
           (repetition "q" #:result/bare (λ (elems) (string-join elems ""))
                       #:before "b" #:greedy? #f))
     (list "" "q" "qq" "qqq"))
  (c check se?
     (p*/r "qqqz"
           (repetition "q" #:result/stx (λ (elems) (string-join elems ""))
                       #:after "z" #:greedy? #f))
     (list (datum->syntax #f "qqq" (list 'string 1 0 1 4))))

  ;;; sequence with begin/before/after
  (c check se?
     (p*/r "a_b_c"
           (sequence "a" "b" "c" #:result/stx (λ elems (string-join elems ""))
                     #:between "_"))
     (list (datum->syntax #f "abc" (list 'string 1 0 1 5))))
  (c check-equal?
     (p*/r "_a_b_c_"
           (sequence "a" "b" "c" #:result/bare (λ elems (string-join elems ""))
                     #:before #t
                     #:after #t
                     #:between "_"))
     (list "abc"))
  (c check-equal?
     (p*/r "^a_b_c$"
           (sequence "a" "b" "c" #:result/bare (λ elems (string-join elems ""))
                     #:before "^"
                     #:after "$"
                     #:between "_"))
     (list "abc"))
  (c check-equal?
     (p*/r "^abc$"
           (sequence "a" "b" "c" #:result/bare (λ elems (string-join elems ""))
                     #:before "^"
                     #:after "$"))
     (list "abc"))


  (c check-equal?
     (p*/r "a"
           (sequence (epsilon-parser) "a" #:result/bare (λ (a b) b)))
     (list "a"))

  (c check-equal?
     (p*/r ""
           eof-parser)
     (list eof))

  (c check-equal?
     (p*/r "abc"
           (not-parser eof-parser #:result 'foo))
     (list 'foo))

  (c se/datum?
     (p*/r "abc"
           (sequence (peek-parser "abc") "abc"))
     (list #'("abc" "abc")))


  ;;;;;;;;;;;;;;;;;;;


  (define whitespace-char-func
    (λ (port)
      (define c (peek-char port))
      (or (and (member c '(#\newline #\space #\return #\tab))
               (begin
                 (read-char port)
                 (make-parse-derivation c #:end (port->pos port))))
          (make-parse-failure #:message "not whitespace"))))
  (define whitespace-char-parser
    (proc-parser #:name "whitespace-char" whitespace-char-func))

  (define symbol-char-func
    (λ (port)
      (define c (peek-char port))
      (or (and (not (member c '(#\newline #\space #\return #\tab #\( #\))))
               (not (eof-object? c))
               (begin
                 (read-char port)
                 (make-parse-derivation c #:end (port->pos port))))
          (make-parse-failure #:message "not symbol char"))))

  (define symbol-char-parser (proc-parser #:name "symbol-char"
                                          symbol-char-func))
  (define symbol-parser (kleene-plus symbol-char-parser
                                     #:name "symbol"
                                     #:greedy? #f
                                     #:result/bare (λ (chars)
                                                     (string->symbol
                                                      (apply string chars)))))

  (define (list-parser) (sequence #:name "list"
                                  #:result/bare (λ (lparen ws1? vals ws2? rparen)
                                                  vals)
                                  "("
                                  (kleene-star whitespace-char-parser
                                               #:result/bare (λ (ws) #f)
                                               #:greedy? #f)
                                  (kleene-star basic-s-exp
                                               #:between (kleene-plus
                                                          whitespace-char-parser
                                                          #:result/bare #t)
                                               #:result/bare #t
                                               #:greedy? #f)
                                  (kleene-star whitespace-char-parser
                                               #:result/bare (λ (ws) #f)
                                               #:greedy? #f)
                                  ")"))

  (define (basic-s-exp)
    (make-alt-parser "s-exp"
                     (list symbol-parser
                           list-parser)))

  (c check-equal?
     (p*/r "()" basic-s-exp)
     (list '()))

  (c check-equal?
     (p*/r "test" basic-s-exp)
     '(t te tes test))

  (define s-exp-str-1 "(test test (test test) test (hello foo (bar aoeu)
                                   thaoneuth)
      uetoannt aueonth aueont huaeonth (uaet hhaoeu)
      atnuoeh (ntaoheu tna oh
                       aotneu uenta
                       aeuont (aonethtnhaueo aunoet auoetnh)
                       thaeuo)
      anoteunthun aoentu oeau)")

  (c check-equal?
     (p*/r s-exp-str-1 basic-s-exp)
     (list (read (open-input-string s-exp-str-1))))

  (c check-equal?
     (length
      (p*/r "aaaa"
            (regexp->parser #px"a*")))
     1)

  (c check-equal?
     (length
      (p*/r "aaaa"
            (repetition "a" #:greedy? #t)))
     1)
  (c check-equal?
     (p*/r "aaaa"
           (repetition "a" #:greedy? #t
                       #:result/bare #t))
     (list (list "a" "a" "a" "a")))
  (c check-equal?
     (p*/r "_aaaa"
           (repetition "a" #:greedy? #t
                       #:before "_"
                       #:result/bare #t))
     (list (list "a" "a" "a" "a")))
  (c check-equal?
     (p*/r "aaaa_"
           (repetition "a" #:greedy? #t
                       #:after "_"
                       #:result/bare #t))
     (list (list "a" "a" "a" "a")))
  (c check-equal?
     (p*/r "a-a-a-a"
           (repetition "a" #:greedy? #t
                       #:between "-"
                       #:result/bare #t))
     (list (list "a" "a" "a" "a")))
  (c check-equal?
     (p*/r "_a-a-a-a_"
           (repetition "a" #:greedy? #t
                       #:between "-"
                       #:before "_"
                       #:after "_"
                       #:result/bare #t))
     (list (list "a" "a" "a" "a")))


  ;; check that sequences handle internal ambiguity properly
  (define (make-a-parser)
    (proc-parser (λ (port)
                   (define s (read-string 1 port))
                   (if (equal? s "a")
                       (make-parse-derivation s #:end (port->pos port))
                       (make-parse-failure #:message "didn't match «a»")))))
  (define two-a-parser
    (make-alt-parser "two-a" (list (make-a-parser) (make-a-parser))))
  (c check-equal?
     (p*/r "ab"
           (sequence two-a-parser "b"
                     #:result/bare (λ (r1 r2) (string-append r1 r2))))
     '("ab" "ab"))


  (check se/datum?
         (p*/r "foo"
               (as-syntax "foo"))
         (list #'"foo"))


  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Filters


#|
TODO - I need a parse stream object that is lazy and holds on to errors so that I can take a successful stream and filter it into an unsuccessful stream while keeping the failures.


TODO - what kind of filters do I need?
* simple predicate filters
** I think simple predicates are enough for precedence and associativity filters...
* filters that read more of the port
** needed for follow filters
* ? filters that compare multiple elements of a stream (eg. a filter that only gives the largest among multiple parses?)
|#

(define (parse-filter parser
                      ;; filter-func is (-> port derivation bool-or-new-derivation)
                      filter-func
                      #:replace-derivation? [replace-derivation? #f]
                      #:include-failures? [include-failures? #t])
  (proc-parser
   #:name (format "~a/filtered" (parser-name parser))
   #:prefix (parser-prefix parser)
   (λ (port)
     (define (rec results failures had-success?)
       (cond [(and (parse-failure? results) (null? failures)) results]
             [(and (parse-failure? results) had-success?)
              (define all-failures (cons results failures))
              (define best-failure (greatest-failure all-failures))
              best-failure]
             [(or (parse-failure? results) (stream-empty? results))
              (define all-failures (if (parse-failure? results)
                                       (cons results failures)
                                       failures))
              (make-parse-failure #:failures all-failures)]
             [else (define r1 (stream-first results))
                   (define filter-result (filter-func port r1))
                   (define use-result (if (and replace-derivation? filter-result)
                                          filter-result
                                          r1))
                   (if filter-result
                       (parse-stream-cons use-result
                                          (rec (stream-rest results) failures #t))
                       (rec (stream-rest results)
                            (if include-failures?
                                (cons (make-parse-failure
                                       #:message
                                       (format "did not pass filter: ~v" r1)
                                       #:position
                                       (parse-derivation-end-position r1))
                                      failures)
                                failures)
                            had-success?))]))
     (rec (parse* port parser) '() #f))
   #:preserve-prefix? #t
   #:promise-no-left-recursion? (and (not (parser-potentially-left-recursive? parser))
                                     (not (parser-potentially-null? parser)))))

(define (follow-filter main-parser not-follow-parser
                       #:include-failures? [include-failures? #t])
  (parse-filter main-parser
                (λ (port result)
                  (define r-follow (parse* port not-follow-parser #:start result))
                  (if (parse-failure? r-follow) #t #f))
                #:include-failures? include-failures?))

(define (derivation-filter parser filter-func
                           #:replace-derivation? [replace-derivation? #f]
                           #:include-failures? [include-failures? #t])
  ;; filter-func is of type (-> derivation bool-or-new-derivation)
  (parse-filter parser (λ (port derivation)
                         (filter-func derivation))
                #:replace-derivation? replace-derivation?
                #:include-failures? include-failures?))
(define (result-filter parser filter-func
                       #:replace-result? [replace-result? #f]
                       #:include-failures? [include-failures? #t])
  ;; filter-func is of type (-> result bool-or-new-result)
  (parse-filter parser
                (λ (port derivation)
                  (let ([filter-result (filter-func
                                        (parse-derivation-result derivation))])
                    (if replace-result?
                        (make-parse-derivation filter-result
                                               #:derivations (list derivation))
                        filter-result)))
                #:replace-derivation? replace-result?
                #:include-failures? include-failures?))


(module+ test

  (c check-equal?
     (p*/r "aaab"
           (repetition (follow-filter "a" "b") #:greedy? #t #:result/bare #t))
     (list (list "a" "a")))

  (c check-equal?
     (p*/r "testing"
           (parse-filter "testing" (λ (port derivation) #t)))
     (list "testing"))
  (c check-equal?
     (p*/r "testing"
           (parse-filter #:replace-derivation? #t
                         "testing"
                         (λ (port derivation)
                           (make-parse-derivation
                            "a dog, a frog, a log"
                            #:derivations (list derivation)))))
     (list "a dog, a frog, a log"))
  (c check-equal?
     (p*/r "testing"
           (parse-filter "testing" (λ (port result) #f)))
     '())

  (check se/datum?
         (->results (whole-parse* (open-input-string "aaa")
                                  (kleene-star "a")))
         (list #'("a" "a" "a")))
  (check se/datum?
         (->results (whole-parse* (open-input-string "aaaa")
                                  (derivation-filter
                                   (kleene-star "a")
                                   (λ (d)
                                     (even? (length
                                             (syntax->list
                                              (parse-derivation-result d))))))))
         (list #'("a" "a" "a" "a")))
  (check se/datum?
         (->results (whole-parse* (open-input-string "aaa")
                                  (derivation-filter
                                   (kleene-star "a")
                                   (λ (d)
                                     (even? (length
                                             (syntax->list
                                              (parse-derivation-result d))))))))
         (list))
  (check se/datum?
         (->results (whole-parse* (open-input-string "aaaa")
                                  (derivation-filter
                                   #:replace-derivation? #t
                                   (kleene-star "a")
                                   (λ (d)
                                     (let ([r (length
                                               (syntax->list
                                                (parse-derivation-result d)))])
                                       (and (even? r)
                                            (make-parse-derivation
                                             r #:derivations d)))))))
         (list 4))
  (check se/datum?
         (->results (whole-parse* (open-input-string "aaaa")
                                  (result-filter
                                   #:replace-result? #t
                                   (kleene-star "a")
                                   (λ (r)
                                     (let ([len (length (syntax->list r))])
                                       (and (even? len) len))))))
         (list 4))


  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Misc useful parsers


(define (->char c)
  (match c
    [(? char? c) c]
    [(and (? string?) (? (λ (x) (eq? (string-length x) 1))))
     (string-ref c 0)]))

(define (char-parser c)
  (define c-use (->char c))
  (proc-parser
   #:promise-no-left-recursion? #t
   (λ (port)
     (if (eq? (read-char port) c-use)
         (make-parse-derivation c-use)
         (make-parse-failure)))))

(define (char-range-parser min [max #f])
  ;; Accepts a min and a max as char or single-char strings,
  ;; OR just min as a string of length 2
  (define-values (min-use max-use)
    (match (list min max)
      [(list (and (? string?) (? (λ (s) (eq? (string-length s) 2))))
             #f)
       (values (string-ref min 0) (string-ref min 1))]
      [(list anything #f) (error 'char-range-parser
                                 "requires min and max value (given ~v, ~v)"
                                 anything #f)]
      [else (values (->char min) (->char max))]))
  (when (char<? max-use min-use)
    (error 'char-range-parser "max (~v) is less than min (~v)" max-use min-use))
  (proc-parser
   #:promise-no-left-recursion? #t
   (λ (port)
     (define c (read-char port))
     (cond [(eof-object? c) (make-parse-failure #:message "got eof")]
           [(char<=? min-use c max-use) (make-parse-derivation c)]
           [else (make-parse-failure
                  #:message (string->immutable-string
                             (format "character (~v) not in range: ~v-~v"
                                     c min-use max-use)))]))))

(define any-char-parser
  (proc-parser
   #:promise-no-left-recursion? #t
   (λ (port)
     (define c (read-char port))
     (if (eof-object? c)
         (make-parse-failure)
         (make-parse-derivation c)))))

(module+ test
  (check-equal? (p*/r "a" (char-parser "a")) '(#\a))
  (check-pred parse-failure? (parse* (open-input-string "b") (char-parser "a")))
  (check-equal? (p*/r "d" (char-range-parser "af")) '(#\d))
  (check-equal? (p*/r "d" (char-range-parser #\a "f")) '(#\d))
  (check-pred parse-failure? (parse* (open-input-string "g")
                                     (char-range-parser "af")))
  (check-equal? (p*/r "d" any-char-parser) '(#\d))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Parsing API.
;; This doesn't really belong here, perhaps.  Meh.

(define (whole-parse* port/pbw parser
                      #:start [start #f])
  (parse* port/pbw (follow-filter parser (not-parser eof-parser)
                                  #:include-failures? #f)))

(define (no-ambiguity+read-port! port result errname)
  (define result*
    (cond [(stream-empty? result) result]
          [(stream-empty? (stream-rest result)) (stream-first result)]
          ;; TODO - better error message
          [else (error errname "ambiguous parse: got (at least):\n~v\nand\n~v"
                       (parse-derivation-result
                        (stream-first result))
                       (parse-derivation-result
                        (stream-first (stream-rest result))))]))
  (when (and (not (stream? result*))
             (input-port? port))
    (read-string (- (parse-derivation-end-position result*)
                    (parse-derivation-start-position result*))
                 port))
  result*)

(define (whole-parse port parser)
  (no-ambiguity+read-port! port (whole-parse* port parser) 'whole-parse))
(define (parse port parser)
  (no-ambiguity+read-port! port (parse* port parser) 'parse))

(module+ test
  (c check-equal?
     (->results
      (whole-parse* (open-input-string "abcd")
                    "abcd"))
     (list "abcd"))
  (c check-equal?
     (->results
      (whole-parse* (open-input-string "abcdefg")
                    "abcd"))
     (list))
  )
