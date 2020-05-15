#lang info
(define collection 'multi)
(define deps
  '("base"
    "scribble-lib"
    "racket-doc"
    "rackunit-lib"
    "data-lib"
    "kw-make-struct"
    "quickcheck"

    ;; For parenhp demo
    "web-server-lib"
    ;; for at-exp demo
    "at-exp-lib"

    ;; Temporarily, to do profiling
    "profile-lib"
    ))

(define version "0.0")
