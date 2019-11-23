#lang racket/base


(provide
 binding-sequence
 (for-syntax binding-sequence-elem)
 )

(require
 "core.rkt"
 "procedural-combinators.rkt"
 racket/match
 racket/stxparam
 (for-syntax
  racket/base
  syntax/parse
  ))

(module+ test
  (require
   rackunit
   "test-util-3.rkt"
   ))

(define (do-ignore-and-splice derivations
                              ignores
                              splices)
  ;; Helper function for binding-sequence
  (define (result->list r)
    (cond [(and (syntax? r) (syntax->list r))]
          [(list? r) r]
          [else (error 'binding-sequence
                       "result to be spliced is not a list: ~v"
                       r)]))
  (define (do-splices result n)
    (if (< n 2)
        (result->list result)
        (apply append
               (map (λ (x) (do-splices x (sub1 n)))
                    (result->list result)))))
  (cond [(null? derivations) '()]
        [(car ignores) (do-ignore-and-splice (cdr derivations)
                                             (cdr ignores)
                                             (cdr splices))]
        [(and (car splices) (< 0 (car splices)))
         (let* ([r (parse-derivation-result (car derivations))]
                [this-list (do-splices r (car splices))])
           (append this-list (do-ignore-and-splice (cdr derivations)
                                                   (cdr ignores)
                                                   (cdr splices))))]
        [else (cons (parse-derivation-result (car derivations))
                    (do-ignore-and-splice (cdr derivations)
                                          (cdr ignores)
                                          (cdr splices)))]))

(define (do-full-sequence-splice val n)
  (match n
    [#f val]
    [0 val]
    [(and (? number?) (? (λ (x) (< 0 x))))
     (match val
       [(list x) (do-full-sequence-splice x (sub1 n))]
       [(? syntax?) (do-full-sequence-splice (syntax-e val) n)]
       [else (error
              'binding-sequence
              "can't do full-sequence splice of something that is not a list of a single element: ~v"
              val)])]))

(begin-for-syntax
  (define-syntax-class binding-sequence-elem
    (pattern (~and whole-pattern
                   [(~datum :)
                    (~or (~once parser:expr)
                         (~optional (~seq #:bind name:id))
                         (~optional (~seq #:ignore ignore-given:expr))
                         (~optional (~seq #:splice splice-given:expr))
                         (~optional (~seq #:repeat-min repeat-min-given:expr))
                         (~optional (~seq #:repeat-max repeat-max-given:expr))
                         (~optional (~seq #:repeat-greedy? repeat-greedy-given:expr))
                         )
                    ...])
             #:attr ignore #'(~? ignore-given #f)
             #:attr splice #'(~? splice-given #f)
             #:attr repeat-min #'(~? repeat-min-given #f)
             #:attr repeat-max #'(~? repeat-max-given #f)
             #:attr repeat-greedy #'(~? repeat-greedy-given #f)
             )
    (pattern parser:expr
             #:attr name #f
             #:attr ignore #'#f
             #:attr splice #'#f
             #:attr repeat-min #'#f
             #:attr repeat-max #'#f
             #:attr repeat-greedy #'#f
             )))

(define-syntax (hidden-between-propagate-key stx)
  (raise-syntax-error 'hidden-between-propagate-key
                      "only exists for use as a hidden keyword"
                      stx))
(define-syntax-parameter binding-subsequence-layout #f)

(define-syntax (binding-sequence-helper stx)
  (syntax-parse stx
    [(_ port derive make-result/bare make-result/stx splice start-point
        between-propagate
        ([done-int-names ...] [done-ignores ...] [done-splices ...])
        [internal-name:id part:binding-sequence-elem]
        rest ...)
     #'(let* ([repeat-min* (~? part.repeat-min #f)]
              [repeat-max* (~? part.repeat-max #f)]
              [repeat-greedy? (~? part.repeat-greedy #f)]
              [repeat (or repeat-min* repeat-max*)]
              [repeat-min (match repeat-min* [#f 0] [#t 0] [else repeat-min*])]
              [repeat-max (match repeat-max*
                            [#f +inf.0] [#t +inf.0] [else repeat-max*])]
              [parser/no-repeat
               ;; TODO - right now this is making ALL subsequences inherit the between argument.  I don't like it.  I should have some specific form recognized by binding sequences to signify a subsequence that inherits.
               (syntax-parameterize
                   ([binding-subsequence-layout #'between-propagate])
                 part.parser)]
              [parser/repeat
               (if repeat
                   (repetition parser/no-repeat
                               #:min repeat-min
                               #:max repeat-max
                               #:between between-propagate
                               #:greedy? repeat-greedy?
                               #:result/bare (not (not make-result/bare))
                               #:result/stx (not (not make-result/stx)))
                   parser/no-repeat)])
         (for/parse ([internal-name (parse* port parser/repeat #:start start-point)])
                    (~? (define part.name internal-name) (void))
                    (define current-ignore part.ignore)
                    (define current-splice (or part.splice 0))
                    (when (not (integer? current-splice))
                      (error 'binding-sequence
                             "splice argument is not an integer: ~v"
                             current-splice))
                    (binding-sequence-helper port
                                             derive
                                             make-result/bare
                                             make-result/stx
                                             splice
                                             internal-name
                                             between-propagate
                                             ([done-int-names ... internal-name]
                                              [done-ignores ... current-ignore]
                                              [done-splices ... current-splice])
                                             rest ...)))]
    [(_ port derive make-result/bare make-result/stx splice start-point
        between-propagate
        ([done-int-names ...] [done-ignores ...] [done-splices ...]))
     #'(let* ([derive-arg derive]
              [make-result/bare-arg make-result/bare]
              [make-result/stx-arg make-result/stx]
              [ignored-spliced-thunk
               (λ () (do-ignore-and-splice (list done-int-names ...)
                                           (list done-ignores ...)
                                           (list done-splices ...)))]
              [make-result-wrap
               (λ (make-result-func stx?)
                 (λ derivations
                   (make-parse-derivation
                    (λ (src line col pos span ds)
                      (let* ([pre-stx+splice (apply make-result-func
                                                    (ignored-spliced-thunk))]
                             [pre-stx (do-full-sequence-splice pre-stx+splice
                                                               splice)])
                        (if stx?
                            (datum->syntax #f pre-stx (list src line col pos span))
                            pre-stx)))
                    #:derivations derivations)))]
              [do-derive (cond [derive-arg derive-arg]
                               [(eq? #t make-result/bare-arg)
                                (make-result-wrap list #f)]
                               [make-result/bare-arg
                                (make-result-wrap make-result/bare-arg #f)]
                               [(eq? #t make-result/stx-arg)
                                (make-result-wrap list #t)]
                               [make-result/stx-arg
                                (make-result-wrap make-result/stx-arg #t)]
                               [else (make-result-wrap list #t)])])
         (apply do-derive (list done-int-names ...)))]))
(define-syntax (binding-sequence stx)
  (syntax-parse stx
    [(_ (~optional (~seq (~literal hidden-between-propagate-key)
                         between-propagate:expr))
        part:binding-sequence-elem ...+
        (~or (~optional (~seq #:name name:expr))
             (~optional (~seq #:derive derive-arg:expr))
             (~optional (~seq #:result/bare make-result-bare-arg:expr))
             (~optional (~seq #:result/stx make-result-stx-arg:expr))
             (~optional (~seq #:splice splice-arg:expr))
             (~optional (~seq #:inherit-between inherit-between:boolean))
             (~optional (~seq #:between between-arg:expr)))
        ...)
     (define (add-betweens between-parser-stx partlist)
       (if (null? partlist)
           '()
           (cons #`[: #,between-parser-stx #:ignore #t]
                 (cons (car partlist)
                       (add-betweens between-parser-stx (cdr partlist))))))
     (define subsequence-layout
       (syntax-parse #'(~? inherit-between #f)
         [#f #'#f]
         [#t (or (syntax-parameter-value #'binding-subsequence-layout) #'#f)]))
     (define/syntax-parse between-arg/use
       (or (attribute between-arg)
           subsequence-layout))
     (if (syntax-parse #'between-arg/use [#f #f] [else #t])
         #`(let ([between-arg* between-arg/use])
             #,(with-syntax ([(parts-with-betweens ...)
                              (cons (car (syntax->list #'(part ...)))
                                    (add-betweens #'between-arg*
                                                  (cdr (syntax->list
                                                        #'(part ...)))))])
                 ;; Allowing syntax-time #f or an expr that must evaluate to
                 ;; a parser is a terrible interface.  So we will allow a
                 ;; second chance to evaluate to false and get a non-between
                 ;; sequence.
                 #'(syntax-parameterize ([binding-subsequence-layout #f])
                     (if between-arg*
                         (binding-sequence hidden-between-propagate-key between-arg*
                                           parts-with-betweens ...
                                           #:name (~? name #f)
                                           #:derive (~? derive-arg #f)
                                           #:result/bare (~? make-result-bare-arg #f)
                                           #:result/stx (~? make-result-stx-arg #f))
                         (binding-sequence part ...
                                           #:name (~? name #f)
                                           #:derive (~? derive-arg #f)
                                           #:result/bare (~? make-result-bare-arg #f)
                                           #:result/stx (~? make-result-stx-arg #f))))))
         (with-syntax ([(internal-name ...) (generate-temporaries #'(part ...))]
                       [sequence-length
                        (datum->syntax #'here
                                       (length (syntax->list #'(part ...))))])
           #'(let ([between-propagate/use (~? between-propagate #f)]
                   [make-result/bare (~? make-result-bare-arg #f)]
                   [make-result/stx (~? make-result-stx-arg #f)]
                   [splice (~? splice-arg #f)])
               (proc-parser #:name (or (~? name #f) "TODO-binding-seq-name")
                            ;; TODO - other optional args
                            (λ (port)
                              (binding-sequence-helper
                               port
                               (~? derive-arg #f)
                               make-result/bare
                               make-result/stx
                               splice
                               #f
                               between-propagate/use
                               ([] [] [])
                               [internal-name part] ...))))))]))



(module+ test

  (check-equal? (p*/r "ab"
                      (binding-sequence "a" "b" #:result/bare #t))
                '(("a" "b")))
  (check-equal? (p*/r "ab"
                      (binding-sequence "a" "b"
                                        #:result/bare #t))
                '(("a" "b")))
  (check se?
         (p*/r "ab"
               (binding-sequence "a" "b"
                                 #:result/stx list))
         (list (datum->syntax #f '("a" "b") (list 'string 1 1 1 2))))
  (check se?
         (p*/r "ab"
               (binding-sequence "a" "b"
                                 #:result/stx #t))
         (list (datum->syntax #f '("a" "b") (list 'string 1 1 1 2))))

  (check-equal? (p*/r "ab"
                      (binding-sequence [: #:bind a1 "a"] "b"
                                        #:result/bare #t))
                '(("a" "b")))
  (check-equal? (p*/r "ab"
                      (binding-sequence [: #:ignore #t "a"] "b"
                                        #:result/bare #t))
                '(("b")))
  (check-equal? (p*/r "a_b"
                      (binding-sequence [: #:ignore #t "a"] "b"
                                        #:between "_"
                                        #:result/bare #t))
                '(("b")))
  (check-equal? (p*/r "aaab"
                      (binding-sequence [: #:splice 1
                                           (kleene-star "a"
                                                        #:result/bare #t)]
                                        "b"
                                        #:result/bare #t))
                '(("a" "a" "a" "b")))
  (check-equal? (p*/r "aaa_b"
                      (binding-sequence [: #:splice 1
                                           (kleene-star "a"
                                                        #:result/bare #t)]
                                        "b"
                                        #:between "_"
                                        #:result/bare #t))
                '(("a" "a" "a" "b")))
  (check-equal? (p*/r "a-a-a_b"
                      (binding-sequence [: #:splice 1 (kleene-star
                                                       "a"
                                                       #:between "-"
                                                       #:result/bare #t)]
                                        "b"
                                        #:between "_"
                                        #:result/bare #t))
                '(("a" "a" "a" "b")))

  (define bseq-a-not-a-parser
    (binding-sequence [: #:bind a1 "a"]
                      (parse-filter (proc-parser (λ (port)
                                                   (make-parse-derivation
                                                    (read-string 1 port)
                                                    #:end (port->pos port))))
                                    (λ (port derivation)
                                      (not (equal?
                                            (parse-derivation-result a1)
                                            (parse-derivation-result derivation)))))
                      #:result/bare #t))
  (check-equal? (p*/r "ab"
                      bseq-a-not-a-parser)
                '(("a" "b")))
  (check-equal? (p*/r "aa"
                      bseq-a-not-a-parser)
                '())

  ;; check binding-sequence repeat
  (check-equal? (p*/r "a_a_a_b"
                      (binding-sequence [: #:splice 1 #:repeat-min 0 "a"]
                                        "b"
                                        #:between "_"
                                        #:result/bare #t))
                '(("a" "a" "a" "b")))
  ;; check binding-sequence subsequnce inheritance of #:between
  (check-equal?
   (p*/r "a_1_2_3_7-8-9_b"
         (binding-sequence "a"
                           [: #:splice 1 (binding-sequence "1" "2" "3"
                                                           #:inherit-between #t
                                                           #:result/bare #t)]
                           [: #:splice 1 (binding-sequence "7" "8" "9"
                                                           #:inherit-between #t
                                                           #:between "-"
                                                           #:result/bare #t)]
                           "b"
                           #:between "_"
                           #:result/bare #t))
   '(("a" "1" "2" "3" "7" "8" "9" "b")))
  (check-equal?
   (p*/r "a_1_2_3_1_2_3_b"
         (binding-sequence "a"
                           [: #:splice 2 #:repeat-min 0
                              (binding-sequence "1" "2" "3"
                                                #:inherit-between #t
                                                #:result/bare #t)]
                           "b"
                           #:between "_"
                           #:result/bare #t))
   '(("a" "1" "2" "3" "1" "2" "3" "b")))

  )
