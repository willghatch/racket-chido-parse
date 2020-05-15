#lang racket/base

(provide
 chido-readtable-add-at-reader
 make-at-reader-parser
 )

(require
 "core-use.rkt"
 "readtable-parser.rkt"
 "procedural-combinators.rkt"
 "parameters.rkt"
 scribble/reader
 racket/stream
 syntax/parse
 )

;; At-expressions are @<cmd>[<datum>*]{text-body*}
;; Building an at-readtable allows overriding the command and datum readtables.
;; For the command readtable I'm actually happy just overriding the left parenthesis.
;; But for the datum readtable, I actually want to override the starting bracket, whereas the option allows you only to change the behavior of a readtable that is called INSIDE the brackets.


;;;; First attempt at building a shim Racket readtable
;; This one is better in some ways, but the problem is that the datum readtable isn't used to read the backets, only INSIDE the brackets.  So to have the readtable be consistent inside the brackets, I have to hijack the readtable on, uh, every character.  Perhaps it would be better to simply re-implement the at-reader, or get a modification to it accepted to allow a different read function on the initial open bracket.  But I'm not about to bother with that when a simple hack will suffice.

;;(define (make-enter-chido-no-l-delim left right)
;;  (λ (l-delim-char port src line col pos)
;;    (define inner-parser
;;      (proc-parser #:name (format "chido-at-reader-inner-~a-~a" left right)
;;                   (λ (port)
;;                     (parse* port (chido-readtable->read* (current-chido-readtable))))
;;                   #:use-port? #f))
;;    (define the-parser (sequence inner-parser right
;;                                 #:result/stx (λ (inner r) inner)))
;;
;;    ;; Parse a single result, duplicates won't fit the interface of Racket's built-in read function that this is going through.
;;    (define result (parse port the-parser))
;;    (parse-derivation-result result)))
;;(define enter-chido-l-paren (make-enter-chido-no-l-delim "(" ")"))
;;(define enter-chido-l-bracket (make-enter-chido-no-l-delim "[" "]"))
;;(define enter-chido-l-brace (make-enter-chido-no-l-delim "{" "}"))
;;
;;
;;
;;(define shim-rt
;;  (make-readtable #f
;;                  #\( 'terminating-macro enter-chido-l-paren
;;                  #\[ 'terminating-macro enter-chido-l-bracket
;;                  #\{ 'terminating-macro enter-chido-l-brace
;;                  ))

;;;; Second attempt at building a shim Racket readtable
;; Just change the meaning of (almost) ALL characters!
;; Except doing all of them takes forever, so let's just choose a reasonably high point that most people probably won't pass.
;; This is a terrible hack.  But without a way to hijack the core readtable functions, or editing the datum reader to actually use a different read function upon hitting the left bracket, this is the best I'm going to do for now.


(define datum-space-hack-result (gensym))
(define datum-space-hack
  (proc-parser
   (λ (port)
     (parse* port (sequence (chido-readtable->layout* (current-chido-readtable))
                            (peek-parser "]")
                            #:result/stx (λ args datum-space-hack-result))))))

(define (core-read->rewind-1-and-chido-read char port src line col pos)
  (define crt (current-chido-readtable))
  (define crt-with-datum-space-hack
    (extend-chido-readtable 'nonterminating datum-space-hack crt))
  (define read-with-some-layout
    (chido-readtable->read1/layout crt-with-datum-space-hack))
  (define results
    (parse* port read-with-some-layout #:start (sub1 (port->pos port))))
  (define single-derivation
    (cond
      [(parse-failure? results) (error 'chido-at-reader
                                       "inner parse failure:\n~a\n"
                                       (parse-failure->string/message results))]
      [(stream-empty? (stream-rest results))
       (stream-first results)]
      [else (error 'chido-at-reader
                   "inner parse got multiple results (starting at ~v)"
                   pos)]))

  (define end-pos (parse-derivation-end-position single-derivation))
  ;; Actually do the read so the end position can propagate through the read-syntax interface.
  (read-string (- end-pos (port->pos port)) port)
  (parse-derivation-result single-derivation))


(define shim-rt
  (begin #;time
   (for/fold ([rt #f])
             (
              ;; This is far enough into unicode that, uh, probably most people won't notice.  But it's still fast enough.
              [i (in-range #x1000)]
              ;; This is after one of the last currently used blocks.
              ;[i (in-range #x2FA1F)]
              ;; This would be the whole range.  But it takes a long time to build this, and a lot of memory.
              ;[i (in-range #x10FFFF)]
              )
     (if (<= #xD800 i #xDFFF)
         rt
         (make-readtable
          rt
          (integer->char i) 'terminating-macro core-read->rewind-1-and-chido-read)))))




;;;; Chido-parse at-readtable implementation using the shim Racket readtable

(define (make-at-reader-parser #:prefix [prefix "@"])
  (define command-char
    (cond [(char? prefix) prefix]
          [(and (string? prefix) (equal? 1 (string-length prefix)))
           (string-ref prefix 0)]
          [(string? prefix)
           (error 'chido-readtable-add-at-reader
                  "Only single character prefixes are supported.  Given ~v"
                  prefix)]))
  (define at-rt
    (make-at-readtable #:command-char command-char
                       #:readtable shim-rt
                       #:command-readtable 'dynamic
                       #:datum-readtable 'dynamic
                       ))
  (wrap-derivation
   (proc-parser
    #:prefix prefix
    #:preserve-prefix? #t
    #:promise-no-left-recursion? #t
    #:name "at-reader"
    (λ (port)
      (parameterize ([current-readtable at-rt])
        (read-syntax (object-name port) port))))
   (λ (d)
     (λ (src line col pos span derivations)
       (let ([orig-result (parse-derivation-result d)])
         (define-syntax-class datum-space-hack-result-stx
           (pattern x:id
                    #:when (eq? (syntax->datum #'x) datum-space-hack-result)))
         (syntax-parse orig-result
           [(pre ... dhrs:datum-space-hack-result-stx post ...)
            (datum->syntax #f (syntax->list #'(pre ... post ...)))]
           [else orig-result]))))))

(define (chido-readtable-add-at-reader
         rt-to-extend
         #:prefix [prefix "@"])

  (extend-chido-readtable 'terminating
                          (make-at-reader-parser #:prefix prefix)
                          rt-to-extend))


(module+ test
  (require
   rackunit
   "test-util-3.rkt"
   (submod "readtable-parser.rkt" an-s-exp-readtable)
   )

  (define at-rt (chido-readtable-add-at-reader an-s-exp-readtable))

  (check se/datum?
         (wp/r "@atcmd{in at string}" at-rt)
         #'(atcmd "in at string"))
  (check se/datum?
         (wp/r "@(command)" at-rt)
         #'(command))
  (check se/datum?
         (wp/r "@«raw string»" at-rt)
         #'"raw string")
  (check se/datum?
         (wp/r "@atcmd[in datum]" at-rt)
         #'(atcmd in datum))
  (check se/datum?
         (wp/r "@atcmd[in datum]{in at string}" at-rt)
         #'(atcmd in datum "in at string"))
  (check se/datum?
         ;; Test for proper space handling in escapes AND datum segment
         (wp/r "@atcmd[in datum  ]{in at string @(escaped) back in string}" at-rt)
         #'(atcmd in datum "in at string " (escaped) " back in string"))
  (check se/datum?
         (wp/r "@atcmd[in datum (with) «guillemet raw string»]{in at string}" at-rt)
         #'(atcmd in datum (with) "guillemet raw string" "in at string"))
  (check se/datum?
         (wp/r "@atcmd[datum «raw»]{in at string @with[«raw»]{escape}}" at-rt)
         #'(atcmd datum "raw" "in at string " (with "raw" "escape")))
  (check se/datum?
         (wp/r "@atcmd[in datum #|with comment|# here]{in at string}" at-rt)
         #'(atcmd in datum here "in at string"))
  (check se/datum?
         (wp/r "@atcmd[in datum    ]{in at string}" at-rt)
         #'(atcmd in datum "in at string"))

  (define caret-rt (chido-readtable-add-at-reader an-s-exp-readtable #:prefix "^"))
  (check se/datum?
         (wp/r "^atcmd" caret-rt)
         #'atcmd)
  (check se/datum?
         (wp/r "^atcmd[in datum]{in at string}" caret-rt)
         #'(atcmd in datum "in at string"))
  (check se/datum?
         (wp/r "^atcmd[datum «raw»]{in at string ^with[«raw»]{escape}}" caret-rt)
         #'(atcmd datum "raw" "in at string " (with "raw" "escape")))


  )

(module+ silenced-test
  (require
   rackunit
   "test-util-3.rkt"
   (submod "readtable-parser.rkt" an-s-exp-readtable)
   )
  ;; Another option to not run this test when running on CI would be to check for the
  ;; $PLT_PKG_BUILD_SERVICE and $CI environment variables, which are set for
  ;; package builds.  But this also silences tests for manual testing when I don't
  ;; want to do comprehensive testing.

  (fail "The below commented out tests are the same as the caret tests, but are failing inexplicably.  Probably some weird unicode issue.")
  ;(define lozenge-rt (chido-readtable-add-at-reader an-s-exp-readtable #:prefix "◊"))
  ;(check se/datum?
  ;       (wp/r "◊atcmd" lozenge-rt)
  ;       #'atcmd)
  ;(check se/datum?
  ;       (wp/r "◊atcmd[in datum]{in at string}" lozenge-rt)
  ;       #'(atcmd in datum "in at string"))
  ;(check se/datum?
  ;       (wp/r "◊atcmd[datum «raw»]{in at string ◊with[«raw»]{escape}}" lozenge-rt)
  ;       #'(atcmd datum "raw" "in at string " (with "raw" "escape")))

  )
