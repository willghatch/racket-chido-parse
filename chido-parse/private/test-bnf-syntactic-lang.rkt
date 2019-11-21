#lang chido-parse/bnf-syntactic

stmt : "pass"
     | expr
     | "{" @ stmt + "}"
expr : $(follow-filter bnumber bnumber)
     | expr "+" expr & left
     | expr "*" expr & left > "+"
bnumber : ($zero-str | "1") +
          :: (λ (elems) (list (apply my-string-append (syntax->datum elems))))

#:definitions
(define my-string-append string-append)
(define zero-str "0")
