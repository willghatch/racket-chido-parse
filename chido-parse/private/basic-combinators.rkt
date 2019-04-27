#lang racket/base

(require
 "scheduler.rkt"
 "forparse.rkt"
 racket/string
 racket/stream
 )

(module+ test
  (require
   rackunit
   (for-syntax
    racket/base
    syntax/parse
    )))

(define (sequence-2 l r
                    #:name [name #f]
                    #:derive [derive #f]
                    #:result [result #f])
  (define prefix (if (proc-parser? l)
                     (proc-parser-prefix l)
                     ""))
  (define use-name (or name (format "sequence-2_~a_~a"
                                    (parser-name l)
                                    (parser-name r))))
  (define combiner
    (cond [(and derive result)
           (error 'sequence-2
                  "must provide either result or derive function, not both")]
          [derive derive]
          [result (λ (l-d r-d)
                    (make-parse-derivation
                     (result (parse-derivation-result l-d)
                             (parse-derivation-result r-d))
                     #:derivations (list l-d r-d)))]
          [else (error 'sequence-2
                       "must provide a result or derivation transformer")]))
  (define (proc port)
    (for/parse ([l-result (parse* port l)])
               (for/parse ([r-result (parse* port r #:start l-result)])
                          (combiner l-result r-result))))
  (proc-parser use-name prefix proc))

(define (sequence #:name [name #f]
                  #:derive [derive #f]
                  #:result [make-result #f]
                  . parsers)
  (define prefix (if (proc-parser? (car parsers))
                     (proc-parser-prefix (car parsers))
                     ""))
  (define use-name (or name (format "sequence_~a"
                                    (string-join (map parser-name parsers) "_"))))
  (define combiner
    (cond [(and derive make-result)
           (error 'sequence
                  "must provide either result or derive function, not both")]
          [derive derive]
          [make-result (λ derivations
                         (make-parse-derivation
                          (apply make-result
                                 (map parse-derivation-result derivations))
                          #:derivations derivations))]
          [else (error 'sequence
                       "must provide a result or derivation transformer")]))
  (define (proc port)
    (define (rec parsers derivations)
      (cond [(null? parsers) (apply combiner (reverse derivations))]
            [else (for/parse ([result (parse* port (car parsers)
                                              #:start (if (null? derivations)
                                                          #f
                                                          (car derivations)))])
                             (rec (cdr parsers) (cons result derivations)))]))
    (rec parsers '()))
  (proc-parser use-name prefix proc))

(define (repetition #:name [name #f]
                    #:derive [derive #f]
                    #:result [make-result #f]
                    #:min [min 0]
                    #:max [max +inf.0]
                    parser)
  (define use-name (or name (format "repeat_~a_~a_~a"
                                    (parser-name parser)
                                    min max)))
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
  (define (proc port)
    (define (get-more-streams derivations)
      (for/parse ([derivation (parse* port parser #:start (if (null? derivations)
                                                              #f
                                                              (car derivations)))])
                 (rec (cons derivation derivations))))
    (define (rec derivations)
      (define len (length derivations))
      (if (< len min)
          (get-more-streams derivations)
          (stream-cons (combiner (reverse derivations))
                       (if (>= len max)
                           empty-stream
                           (get-more-streams derivations)))))
    (rec '()))
  (proc-parser use-name "" proc))

(define (kleene-star #:name [name #f]
                     #:derive [derive #f]
                     #:result [make-result #f]
                     parser)
  (repetition parser
              #:name (or name (format "~a*" (parser-name parser)))
              #:derive derive
              #:result make-result
              #:min 0))

(define (kleene-plus #:name [name #f]
                     #:derive [derive #f]
                     #:result [make-result #f]
                     parser)
  (repetition parser
              #:name (or name (format "~a+" (parser-name parser)))
              #:derive derive
              #:result make-result
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

(define (epsilon-parser #:name [name "epsilon"]
                        #:result-value [result #f])
  (proc-parser name "" (λ (p) (make-parse-derivation result))))


(define (between* main-parser between-parser
                  #:derive [derive #f]
                  #:result [make-result #f])
  ;; TODO - better version
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
    (stream-cons
     (combiner '())
     (for/parse ([d (parse* port main-parser)])
                (stream-cons
                 (combiner (list d))
                 (for/parse ([d2 (parse* port between+main+
                                         #:start d)])
                            (combiner
                             (cons d (parse-derivation-derivation-list d2))))))))
  (proc-parser use-name "" proc))

(define (traditional-read-func->parse-result-func f #:syntax? [syntax? #f])
  (λ (port)
    (define result (if syntax?
                       (f (object-name port) port)
                       (f port)))
    (define-values (line col pos) (port-next-location port))
    (make-parse-derivation result
                           #:end pos
                           #:derivations '())))


(module+ test
  (define-syntax (c stx)
    (syntax-parse stx
      [(_ check-name arg ...)
       (datum->syntax stx
                      (syntax-e #'(check-not-exn (λ () (check-name arg ...))))
                      stx)]))

  (define ap (string->parser "a"))
  (define bp "b")
  (define cp "c")
  (define (Bp) (alt-parser "B"
                           (list bp
                                 (sequence #:name "Bb" #:result string-append
                                           Bp bp))
                           (list '() '())))
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
                 (make-parse-derivation c)))
          (make-parse-failure "not whitespace"))))
  (define whitespace-char-parser
    (proc-parser "whitespace-char" "" whitespace-char-func))

  (define symbol-char-func
    (λ (port)
      (define c (peek-char port))
      (or (and (not (member c '(#\newline #\space #\return #\tab #\( #\))))
               (begin
                 (read-char port)
                 (make-parse-derivation c)))
          (make-parse-failure "not symbol char"))))

  (define symbol-char-parser (proc-parser "symbol-char" "" symbol-char-func))
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
    (alt-parser "s-exp"
                (list symbol-parser
                      list-parser)
                (list '() '())))

  (c check-equal?
     (map parse-derivation-result
          (stream->list
           (parse* (open-input-string "()") basic-s-exp)))
     (list '()))

  (c check-equal?
     (map parse-derivation-result
          (stream->list
           (parse* (open-input-string "test") basic-s-exp)))
     (list 'test))


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

  #|
  S-expression tests:
  (()()()()()) -- this should work -- no spaces are needed between expressions in lists or in the top-level, except to denote boundaries between symbols and numbers.
  |#




  )
