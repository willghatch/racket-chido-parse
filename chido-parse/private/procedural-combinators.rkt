#lang racket/base

(provide
 sequence
 repetition
 kleene-star
 kleene-plus
 epsilon-parser

 ;; TODO - These are not great, should probably be replaced
 ;between*
 traditional-read-func->parse-result-func
 wrap-derivation

 regexp->parser
 )

(require
 "scheduler.rkt"
 "parse-stream.rkt"
 racket/string
 racket/stream
 (for-syntax
  racket/base
  syntax/parse
  ))

(module+ test
  (require
   rackunit
   (for-syntax
    racket/base
    syntax/parse
    )))

(define-syntax (/end stx)
  ;; quick hack to get end positions working with empty derivation lists
  (syntax-parse stx
    [(_ n body)
     #'(parameterize ([current-chido-parse-derivation-implicit-end n])
         body)]))

(define (port->pos port)
  (define-values (line col pos) (port-next-location port))
  pos)

#|
TODO -
I need to re-think the derivation result interface for all these combinators.
|#


(define (sequence #:name [name #f]
                  #:derive [derive #f]
                  #:result [make-result #f]
                  . parsers)
  (define (l-recursive? parsers)
    ;; TODO - I should disallow a null list of parsers...
    (cond [(null? parsers) #f]
          [(parser-potentially-left-recursive? (car parsers)) #t]
          [(parser-potentially-null? (car parsers)) (l-recursive? (cdr parsers))]
          [else #f]))
  (define l-r (l-recursive? parsers))
  ;(define null (andmap parser-potentially-null? parsers))
  (define prefix (parser-prefix (car parsers)))

  (define use-name (or name (format "sequence_~a"
                                    (string-join (map parser-name parsers) "_"))))
  (define combiner
    (cond [(and derive make-result)
           (error 'sequence
                  "must provide either result or derive function, not both")]
          [derive derive]
          [make-result (λ derivations
                         (make-parse-derivation
                          (λ args
                            (apply make-result
                                   (map parse-derivation-result derivations)))
                          #:derivations derivations))]
          [else (error 'sequence
                       "must provide a result or derivation transformer")]))
  (define (proc pb)
    (define (rec parsers derivations)
      (cond [(null? parsers) (/end (port-broker-start-position pb)
                                   (apply combiner (reverse derivations)))]
            [else (for/parse ([result (parse* pb (car parsers)
                                              #:start (if (null? derivations)
                                                          #f
                                                          (car derivations)))])
                             (rec (cdr parsers) (cons result derivations)))]))
    (rec parsers '()))
  (proc-parser #:name use-name prefix proc
               #:promise-no-left-recursion? (not l-r)
               #:preserve-prefix? #t
               #:use-port? #f))

(define (repetition #:name [name #f]
                    #:derive [derive #f]
                    #:result [make-result #f]
                    #:min [min 0]
                    #:max [max +inf.0]
                    #:greedy? [greedy? #f]
                    parser)
  (define use-name (or name (format "repeat_~a_~a_~a"
                                    (parser-name parser)
                                    min max)))
  (define combiner
    (cond [(and derive make-result)
           (error 'repetition
                  "must provide either result or derive function, (and not both)")]
          [derive derive]
          [make-result (λ (derivations)
                         (make-parse-derivation
                          (λ args
                            (make-result (map parse-derivation-result derivations)))
                          #:derivations derivations))]
          [else (λ (derivations)
                  (make-parse-derivation
                   (λ args
                     (map parse-derivation-result derivations))
                   #:derivations derivations))]))
  (define (proc pb)
    (define (finalize reversed-derivations)
      (/end (port-broker-start-position pb)
            (combiner (reverse reversed-derivations))))
    (define (get-more-streams derivations)
      (define next-stream (parse* pb parser #:start (if (null? derivations)
                                                        #f
                                                        (car derivations))))
      (if (and greedy?
               (stream-empty? next-stream)
               (<= min (length derivations)))
          (finalize derivations)
          (for/parse ([derivation next-stream])
                     (rec (cons derivation derivations)))))
    (define (rec derivations)
      (define len (length derivations))
      (cond [(or (< len min)
                 (and greedy? (< len max)))
             (get-more-streams derivations)]
            [else (parse-stream-cons (finalize derivations)
                                     (if (>= len max)
                                         empty-stream
                                         (get-more-streams derivations)))]))
    (rec '()))
  (proc-parser #:name use-name "" proc
               #:promise-no-left-recursion?
               (not (parser-potentially-left-recursive? parser))
               #:use-port? #f))

(define (kleene-star #:name [name #f]
                     #:derive [derive #f]
                     #:result [make-result #f]
                     #:greedy? [greedy? #f]
                     parser)
  (repetition parser
              #:name (or name (format "~a*" (parser-name parser)))
              #:derive derive
              #:result make-result
              #:greedy? greedy?
              #:min 0))

(define (kleene-plus #:name [name #f]
                     #:derive [derive #f]
                     #:result [make-result #f]
                     #:greedy? [greedy? #f]
                     parser)
  (repetition parser
              #:name (or name (format "~a+" (parser-name parser)))
              #:derive derive
              #:result make-result
              #:greedy? greedy?
              #:min 1))

(define (optional #:name [name #f]
                  #:derive [derive #f]
                  #:result [make-result #f]
                  parser)
  (repetition parser
              #:name (or name (format "~a?" (parser-name parser)))
              #:derive derive
              #:result make-result
              #:max 1))

;; TODO - greedy repetition

(define (epsilon-parser #:name [name "epsilon"]
                        #:result [result #f])
  (proc-parser #:name name
               ""
               (λ (p) (make-parse-derivation result
                                             #:end (port-broker-start-position p)))
               #:promise-no-left-recursion? #t
               #:use-port? #f))


(define (between* main-parser between-parser
                  #:derive [derive #f]
                  #:result [make-result #f])
  ;; TODO - better version
  ;; TODO - also, the result handling of this is wrong and doesn't match the new lazy API.
  ;; This is like kleene star for the main parser, except that between any
  ;; two of the main parser, the between parser must succeed, though its
  ;; result is thrown away.
  (define use-name "TODO-between*-name")
  (define combiner
    (cond [(or (and derive make-result)
               (and (not derive) (not make-result)))
           (error 'repetition
                  "must provide either result or derive function, (and not both)")]
          [derive derive]
          [make-result (λ (derivations)
                         (make-parse-derivation
                          (make-result (map parse-derivation-result derivations))
                          #:derivations derivations))]))
  (define between+main (sequence between-parser main-parser
                                 #:result (λ (r1 r2) r2)))
  (define between+main+ (kleene-plus between+main #:result (λ(x)x)))
  (define (proc port)
    (parse-stream-cons
     (/end (port->pos port) (combiner '()))
     (for/parse ([d (parse* port main-parser)])
                (parse-stream-cons
                 (combiner (list d))
                 (for/parse ([d2 (parse* port between+main+
                                         #:start d)])
                            (combiner
                             (cons d (parse-derivation-derivation-list d2))))))))
  (proc-parser #:name use-name "" proc))

(define (wrap-derivation parser wrap-func #:name [name #f])
  (proc-parser #:name (or name (parser-name parser))
               (parser-prefix parser)
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
        (make-parse-failure failure-message #:position pos)))
  (define failure-message (format "Didn't match regexp: ~a" (or name rx)))
  (proc-parser #:name (or name (format "~a" rx))
               ""
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
                                      (sequence #:name "Bb" #:result string-append
                                                Bp bp))))
  (define aBcp (sequence #:name "aBc" #:result string-append
                         ap Bp cp))


  (define str1 "abbbbbbc")
  (define p1 (open-input-string str1))
  (define r1 (parse* p1 aBcp))
  (c check-equal?
     (parse-derivation-result (car (stream->list r1)))
     str1)


  (c check-equal?
     (map parse-derivation-result
          (stream->list
           (parse* (open-input-string "qqq")
                   (kleene-star "q" #:result (λ (elems) (string-join elems ""))))))
     (list "" "q" "qq" "qqq"))
  (c check-equal?
     (map parse-derivation-result
          (stream->list
           (parse* (open-input-string "qqq")
                   (kleene-plus "q" #:result (λ (elems) (string-join elems ""))))))
     (list "q" "qq" "qqq"))
  (c check-equal?
     (map parse-derivation-result
          (stream->list
           (parse* (open-input-string "qqqqqqqqqqqqqqqqqqqqqqq")
                   (repetition "q" #:result (λ (elems) (string-join elems ""))
                               #:min 3 #:max 5))))
     (list "qqq" "qqqq" "qqqqq"))

  (c check-equal?
     (map parse-derivation-result
          (stream->list
           (parse* (open-input-string "q q q")
                   (between* "q" " " #:result (λ (elems) (string-join elems ""))))))
     (list "" "q" "qq" "qqq"))



  ;;;;;;;;;;;;;;;;;;;


  (define whitespace-char-func
    (λ (port)
      (define c (peek-char port))
      (or (and (member c '(#\newline #\space #\return #\tab))
               (begin
                 (read-char port)
                 (make-parse-derivation c #:end (port->pos port))))
          (make-parse-failure "not whitespace"))))
  (define whitespace-char-parser
    (proc-parser #:name "whitespace-char" "" whitespace-char-func))

  (define symbol-char-func
    (λ (port)
      (define c (peek-char port))
      (or (and (not (member c '(#\newline #\space #\return #\tab #\( #\))))
               (begin
                 (read-char port)
                 (make-parse-derivation c #:end (port->pos port))))
          (make-parse-failure "not symbol char"))))

  (define symbol-char-parser (proc-parser #:name "symbol-char"
                                          "" symbol-char-func))
  (define symbol-parser (kleene-plus symbol-char-parser
                                     #:name "symbol"
                                     #:result (λ (chars)
                                                (string->symbol
                                                 (apply string chars)))))

  (define (list-parser) (sequence #:name "list"
                                  #:result (λ (lparen ws1? vals ws2? rparen)
                                             vals)
                                  "("
                                  (kleene-star whitespace-char-parser
                                               #:result (λ (ws) #f))
                                  (between* basic-s-exp
                                            (kleene-plus whitespace-char-parser
                                                         #:result (λ (ws) #f))
                                            #:result (λ (elems) elems))
                                  (kleene-star whitespace-char-parser
                                               #:result (λ (ws) #f))
                                  ")"))

  (define (basic-s-exp)
    (make-alt-parser "s-exp"
                     (list symbol-parser
                           list-parser)))

  (c check-equal?
     (map parse-derivation-result
          (stream->list
           (parse* (open-input-string "()") basic-s-exp)))
     (list '()))

  (c check-equal?
       (map parse-derivation-result
            (stream->list
             (parse* (open-input-string "test") basic-s-exp)))
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
     (map parse-derivation-result
          (stream->list
           (parse* (open-input-string s-exp-str-1) basic-s-exp)))
     (list (read (open-input-string s-exp-str-1))))

  (c check-equal?
     (length
      (stream->list
       (parse* (open-input-string "aaaa")
               (regexp->parser #px"a*"))))
     1)

  (c check-equal?
     (length
      (stream->list
       (parse* (open-input-string "aaaa")
               (repetition "a" #:greedy? #t))))
     1)
  (c check-equal?
     (parse-derivation-result
      (car
       (stream->list
        (parse* (open-input-string "aaaa")
                (repetition "a" #:greedy? #t)))))
     (list "a" "a" "a" "a"))

  #|
  S-expression tests:
  (()()()()()) -- this should work -- no spaces are needed between expressions in lists or in the top-level, except to denote boundaries between symbols and numbers.
  |#




  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Filters


#|
TODO - I need a parse stream object that is lazy and holds on to errors so that I can take a successful stream and filter it into an unsuccessful stream while keeping the failures.


TODO - what kind of filters do I need?
* simple predicate filters
** I think simple predicates are enough for precidence and associativity filters...
* filters that read more of the port
** needed for follow filters
* ? filters that compare multiple elements of a stream (eg. a filter that only gives the largest among multiple parses?)
|#

(define (parse-filter parser
                      ;; filter-func is (-> port result bool-or-new-result)
                      filter-func
                      #:replace-result? [replace-result? #f])
  (proc-parser
   (parser-prefix parser)
   (λ (port)
     (define (rec results)
       (cond [(parse-failure? results) results]
             ;; TODO - I should keep track of failures and be able to return something sensible here.
             [(stream-empty? results) results]
             [else (define r1 (stream-first results))
                   (define filter-result (filter-func port r1))
                   (define use-result (if (and replace-result? filter-result)
                                          filter-result
                                          r1))
                   (if filter-result (parse-stream-cons
                                      use-result
                                      (rec (stream-rest results)))
                       (rec (stream-rest results)))]))
     (rec (parse* port parser)))
   #:preserve-prefix? #t))

(define (follow-filter main-parser not-follow-parser)
  (parse-filter main-parser
                (λ (port result)
                  (define r-follow (parse* port not-follow-parser #:start result))
                  (if (parse-failure? r-follow) #t #f))))


(module+ test

  (c check-equal?
     (parse-derivation-result
      (car
       (stream->list
        (parse* (open-input-string "aaab")
                (repetition (follow-filter "a" "b") #:greedy? #t)))))
     (list "a" "a"))

  )
