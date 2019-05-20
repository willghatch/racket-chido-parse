#lang racket/base

(require
 "scheduler.rkt"
 "basic-combinators.rkt"
 "trie.rkt"
 )


(struct readtable-parser
  (
   ;; Core parsers
   terminating-parsers
   nonterminating-parsers
   layout-parsers

   ;; Options
   symbol-result-transformer
   complex-number-support?
   literal-left-delimiter
   literal-right-delimiter
   symbol-escape ;; IE backslash

   ;; Basically these are cached results
   terminating-trie
   nonterminating-trie
   layout-trie

   symbol/number-parser
   layout*-parser
   read1-parser
   read*-parser
   )
 ; #:prop prop:custom-parser
 ; (λ (self)
 ;   ;; return a parser object
 ;   TODO)
  )

(define empty-readtable-parser
  (readtable-parser
   ;; core parsers
   '() '() '()
   ;; options
   #f
   #f
   ;; TODO - should literal delimiters and backslash be set on the empty readtable, or should they be #f?
   ;; TODO - also, when literal delimiters are NOT equal, should they be nestable?  I think yes.  Maybe there should be an option?
   "|"
   "|"
   "\\"
   ;; tries and parsers
   empty-trie
   empty-trie
   empty-trie

   TODO
   TODO
   TODO
   TODO
   ))

(define (extend-chido-readtable rt extension-type parser)
  ;; extension-type is 'terminating, 'nonterminating, or 'layout
  TODO)

#|
I probably want
* readtable-parser->read-1
* readtable-parser->read-1/no-leading-layout
* readtable-parser->read-multi
|#

(define (readtable-parser->symbol-parser rt)
  TODO)

(define (readtable-parser->number-parser rt)
  TODO)

;; TODO - these are wrong, because I still need to synthesize symbol and number readers.
(define (readtable-parser->read-1/no-leading-layout rt)
  (define ps (append (readtable-parser-terminating-parsers rt)
                     (readtable-parser-nonterminating-parsers rt)))
  (alt-parser ps (map ps '())))
(define (readtable-parser->read-1 rt)
  (define layout-parser
    (alt-parser (readtable-parser-layout-parsers rt)
                (map (λ (x) '()) (readtable-parser-layout-parsers rt))))
  (sequence #:name TODO #:result (λ (a b) b)
            (kleene-star #:name TODO #:result (λ (args) #f) layout-parser)
            (readtable-parser->read-1/no-leading-layout rt)))

#|
TODO - this is basically the list parser but without outer parens.
This should maybe have options for how to deal with leading/trailing whitespace.
I feel like I want a version that will return a stream of sequences starting with reading none to reading all, which will then be filtered by needing a terminator or something.
Actually, I think the list reader should be defined in terms of this, and not the other way around.

|#
(define (readtable-parser->read-multi rt
                                      #:allow-leading-layout [leading-layout #t]
                                      #:allow-trailing-layout [trailing-layout #t]
                                      )
  ;; eat leading layout (by itself)
  ;; read 1
  ;; do (layout* read1)*
  ;; eat trailing layout (by itself)
  TODO)
