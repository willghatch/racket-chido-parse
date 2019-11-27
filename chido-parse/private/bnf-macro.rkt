#lang racket/base
(provide
 define-bnf/syntactic
 #|
 TODO
 define-bnf-arm/syntactic
 readtable-extend-as-bnf-arm/syntactic
 extend-bnf/syntactic
 |#

 ;; for internal use in other chido-parse modules
 define-bnf/syntactic/parsed
 )

(require
 "bnf-s-exp.rkt"
 "procedural-combinators.rkt"
 (for-syntax
  "core.rkt"
  "bnf-s-exp.rkt"
  "procedural-combinators.rkt"
  "bnf-parse.rkt"
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
 (define-syntax-class syntactic-bnf-alt-flag
   (pattern ("&" flag-arg:str)
            #:attr transformed #`(& '#,(datum->syntax #f (string->symbol
                                                          (syntax-e #'flag-arg)))))
   (pattern (flag-name flag-arg)
            #:attr transformed #`(#,(datum->syntax #f (string->symbol
                                                       (syntax-e #'flag-name)))
                                  flag-arg)))
 (define-syntax-class compound-parser
   (pattern (~and whole-stx
                  (~or ((~datum ELEM-ALT) alt-member:syntactic-bnf-elem ...)
                       ((~datum ELEM-LIST) seq-member:syntactic-bnf-elem ...)
                       simple-elem))
            #:attr transformed (syntax/loc
                                   #'whole-stx
                                   (~? (|| (~@ . alt-member.transformed) ...)
                                       (~? #((~@ . seq-member.transformed) ...)
                                           simple-elem)))))
 (define-syntax-class syntactic-bnf-elem
   (pattern ((~datum elem)
             bind-list
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
                                         (make-string (length
                                                       (syntax->list #'(x ...)))
                                                      #\@))))]
                            [else #'()])
            #:attr question (syntax-parse #'question-list [(x) #'(?)] [else #'()])
            #:attr star (syntax-parse #'star-list [(x) #'(*)] [else #'()])
            #:attr plus (syntax-parse #'plus-list [(x) #'(+)] [else #'()])
            #:attr transformed #'((~@ . bind)
                                  (~@ . ignore)
                                  (~@ . splice)

                                  compound-parser.transformed

                                  (~@ . question)
                                  (~@ . star)
                                  (~@ . plus)))
   )
 (define-syntax-class syntactic-bnf-arm-alt
   (pattern ((~datum alt)
             (elem:syntactic-bnf-elem ...)
             (flag:syntactic-bnf-alt-flag ...))
            #:attr transformed #'((~@ . elem.transformed)
                                  ...
                                  (~@ . flag.transformed)
                                  ...)))
 )

(define-syntax (define-bnf/syntactic/parsed stx)
  (syntax-parse stx
    [(_ name
        (~datum top-level)
        ([~datum arm]
         (~or (~optional (~and "/" ignore-arm-name))
              (~optional (~and "%" no-layout-for-arm)))
         ...
         arm-name:id ":" arm-alt:syntactic-bnf-arm-alt ...)
        ...)
     (define/syntax-parse (ignore ...)
       (map (λ (x) (if x #'(/) #'()))
            (attribute ignore-arm-name)))
     (define/syntax-parse (no-layout ...)
       (map (λ (x) (if x #'(%) #'()))
            (attribute no-layout-for-arm)))
     #'(define-bnf/quick name
         [(~@ . ignore)
          (~@ . no-layout)
          arm-name
          arm-alt.transformed ...]
         ...)]))

(define-syntax (define-bnf/syntactic stx)
  (syntax-parse stx
    [(_ name:id src:string)
     (define derivation
       (whole-parse (open-input-string (syntax->datum #'src))
                    (bnf-parser->with-surrounding-layout
                     syntactic-bnf-parser)))
     (when (parse-failure? derivation)
       (eprintf "~a" (parse-failure->string/chain derivation))
       (raise-syntax-error 'define-bnf/syntactic
                           "error while parsing syntax string"
                           #'src))
     (define parse-result
       (replace-context
        #'src
        (parse-derivation-result
         derivation)))
     #`(define-bnf/syntactic/parsed name #,@parse-result)]))


(module+ test

  (define-bnf/syntactic stmt-test "
stmt : \"pass\"
     | expr
     | \"{\" @ stmt + \"}\"
expr : @ $(follow-filter bnumber bnumber)
     | expr \"+\" expr & left
     | expr \"*\" expr & left > \"+\"
/bnumber : (\"0\" | \"1\") +
           :: (λ (elems) (list (apply string-append (syntax->datum elems))))
")


  (check se/datum?
         (wp*/r "pass" stmt-test)
         (list #'(stmt "pass")))
  (check se/datum?
         (wp*/r "0" stmt-test)
         (list #'(stmt (expr "0"))))
  (check se/datum?
         (wp*/r "{pass}" stmt-test)
         (list #'(stmt "{" (stmt "pass") "}")))
  (check se/datum?
         (wp*/r "{ 1 }" stmt-test)
         (list #'(stmt "{" (stmt (expr "1")) "}")))
  (check se/datum?
         (wp*/r "{ 101 }" stmt-test)
         (list #'(stmt "{" (stmt (expr "101")) "}")))

  (check se/datum?
         (wp*/r "{ 10 + 11 pass 11 * 11 + 110}" stmt-test)
         (list #'(stmt  "{"
                        (stmt (expr (expr "10") "+" (expr "11")))
                        (stmt "pass")
                        (stmt (expr (expr (expr "11") "*" (expr "11"))
                                    "+" (expr "110")))
                        "}")))

  )
