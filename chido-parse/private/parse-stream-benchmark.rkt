#lang racket/base
(require
 "parse-stream.rkt"
 racket/stream
 )

(define (sc-nest n thunk)
  (if (eq? 0 n)
      empty-stream
      (stream-cons (thunk)
                   (sc-nest (sub1 n) thunk))))
(define (psc-nest n thunk)
  (if (eq? 0 n)
      empty-stream
      (parse-stream-cons (thunk)
                         (sc-nest (sub1 n) thunk))))

(define repetitions 1000000)

(define (do-trial f)
  (collect-garbage)
  (time (void (stream->list (f repetitions (λ () (random 10)))))))

(module+ main
  (printf "normal\n")
  (do-trial sc-nest)
  (printf "parse-stream-cons\n")
  (do-trial psc-nest)
  (printf "normal\n")
  (do-trial sc-nest)
  (printf "parse-stream-cons\n")
  (do-trial psc-nest)
  (printf "normal\n")
  (do-trial sc-nest)
  (printf "parse-stream-cons\n")
  (do-trial psc-nest)
  )

