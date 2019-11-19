#lang racket/base
(require
 "bnf.rkt"
 (for-syntax
  "core.rkt"
  "procedural-combinators.rkt"
  "quick-bnf-parse.rkt"
  racket/base
  syntax/parse
  syntax/strip-context
  ))

(module+ test
  (require
   rackunit
   "test-util-3.rkt"
   ))

(begin-for-syntax
 (define-syntax-class quick-bnf-alt-flag
   (pattern ("&" flag-arg:str)
            #:attr transformed #`(& '#,(datum->syntax #f (string->symbol
                                                          (syntax-e #'flag-arg)))))
   (pattern (flag-name flag-arg)
            #:attr transformed #`(#,(datum->syntax #f (string->symbol
                                                       (syntax-e #'flag-name)))
                                  flag-arg)))
 (define-syntax-class compound-parser
   (pattern (~or ((~datum ELEM-ALT) alt-member:quick-bnf-elem ...)
                 ((~datum ELEM-LIST) seq-member:quick-bnf-elem ...)
                 simple-elem)
            #:attr transformed #'(~? (|| alt-member.transformed ...)
                                     (~? #(seq-member.transformed ...)
                                         simple-elem))))
 (define-syntax-class quick-bnf-elem
   (pattern (bind-list
             ignore-list
             splice-list

             compound-parser:compound-parser

             question-list
             star-list
             plus-list)
            ;; These are stupid.  Basically I'm converting strings to symbols.  I should have a better way to have specific symbols in the parse above.
            #:attr bind (syntax-parse #'bind-list [(x:id "=") #'(x =)] [else #'()])
            #:attr ignore (syntax-parse #'ignore-list [(x) #'(/)] [else #'()])
            #:attr splice (syntax-parse #'splice-list
                            [(x ...+) (datum->syntax
                                       #f
                                       (list
                                        (string->symbol
                                         (build-string (length
                                                        (syntax->list #'(x ...)))
                                                       "@"))))]
                            [else #'()])
            #:attr question (syntax-parse #'question-list [(x) #'(?)] [else #'()])
            #:attr star (syntax-parse #'question-list [(x) #'(*)] [else #'()])
            #:attr plus (syntax-parse #'question-list [(x) #'(+)] [else #'()])
            #:attr transformed #'((~@ . bind)
                                  (~@ . ignore)
                                  (~@ . splice)

                                  compound-parser.transformed

                                  (~@ . question)
                                  (~@ . star)
                                  (~@ . plus)))
   )
 (define-syntax-class quick-bnf-arm-alt
   (pattern ((elem:quick-bnf-elem ...) (flag:quick-bnf-alt-flag ...))
            #:attr transformed #'((~@ . elem.transformed)
                                  ...
                                  (~@ . flag.transformed)
                                  ...)))
 )

(define-syntax (quick-bnf-definition/parsed stx)
  (syntax-parse stx
    [(_ name
        (arm-name:id ":" arm-alt:quick-bnf-arm-alt ...)
        ...)
     #'(define-bnf/quick name
         [arm-name arm-alt.transformed ...] ...)]))

(define-syntax (define-quick-bnf stx)
  (syntax-parse stx
    [(_ name:id src:string)
     (define parse-result
       (replace-context
        #'src
        (parse-derivation-result
         (whole-parse (open-input-string (syntax->datum #'src))
                      quick-bnf-parser))))
     #`(quick-bnf-definition/parsed name #,@parse-result)]))


(module+ test

  (define-quick-bnf stmt-test "
stmt : \"pass\"
     | expr
     | \"{\" stmt + \"}\"
expr : bnumber
     | expr \"+\" expr & left
     | expr \"*\" expr & left > \"+\"
bnumber : (\"0\" | \"1\") +
")


  (check se/datum?
         (wp*/r "pass" stmt-test)
         (list #'"pass"))
  (check se/datum?
         (wp*/r "{pass}" stmt-test)
         (list #'("{" "pass" "}")))
  (check se/datum?
         (wp*/r "{ 1 }" stmt-test)
         (list #'("{" ("1") "}")))

  #;(check se/datum?
         (wp*/r "{ 10 + 11 pass 11 * 11 + 110}" stmt-test)
         aoeu)

  )
