#lang chido-parse/bnf-syntactic

stmt : "pass"
     | expr
     | "{" @ stmt + "}"
expr : $(follow-filter bnumber bnumber)
     | expr "+" expr & left
     | expr "*" expr & left > "+"
bnumber : ("0" | "1") +
          :: (λ (elems) (list (apply string-append (syntax->datum elems))))
