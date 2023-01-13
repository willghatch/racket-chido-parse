#lang chido-parse/private/test-reader racket/base

#|This is inside a comment...|#
;##{printf "hello from list comment\n"}##

(printf "hello world\n")
(printf "hello again\n")
(printf «hello from raw string
        ~a aoeuaoeu
        » 'testing)
(printf "testing\n")
(printf "foobar\n")
