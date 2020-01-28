#lang racket/base
(provide
 (rename-out
  [stream->stream-stack-cursor stream-flatten]
  [stream-stack-cursor? flattened-stream?]
  [stream-stack-cursor-failures flattened-stream-failures]))

(require
 racket/stream
 "parse-failure.rkt"
 )

#|
A flattened stream is flattened depth-first, a la `flatten` for lists.
|#

(define generic-stream-first stream-first)

(struct stream-stack-cursor (stack failures)
  #:transparent
  #:methods gen:stream
  [(define (stream-empty? ssc)
     (null? (stream-stack-cursor-stack ssc)))
   (define (stream-first ssc)
     (define s (stream-stack-cursor-stack ssc))
     (if (null? s)
         (error 'stream-first "Empty stream")
         (generic-stream-first (car s))))
   (define (stream-rest ssc)
     (define s (stream-stack-cursor-stack ssc))
     (if (null? s)
         (error 'stream-rest "Empty stream")
         (stream-stack-cursor-next ssc)))])


(define (stream-stack-cursor-next ssc)
  (define s (stream-stack-cursor-stack ssc))
  (define failures (stream-stack-cursor-failures ssc))
  (cond [(null? s) ssc]
        [(stream-empty? (car s))
         (define s2 (cdr s))
         (cond [(null? s2) (stream-stack-cursor
                            '()
                            (if (parse-failure? (car s))
                                (cons (car s) failures)
                                failures))]
               [else (stack+v->scc (cdr s2) (stream-rest (car s2)) failures)])]
        [else
         (stack+v->scc (cdr s) (stream-rest (car s)) failures)]))

(define (stack+v->scc stack v failures)
  (cond [(and (stream? v) (not (stream-empty? v)))
         (stack+v->scc (cons v stack) (stream-first v) failures)]
        [(and (stream? v) (null? stack))
         (stream-stack-cursor '() (if (parse-failure? v)
                                      (cons v failures)
                                      failures))]
        [(stream? v)
         (stack+v->scc (cdr stack)
                       (stream-rest (car stack))
                       (if (parse-failure? v)
                           (cons v failures)
                           failures))]
        [else (stream-stack-cursor stack failures)]))

(define (stream->stream-stack-cursor s-tree)
  (stack+v->scc '() s-tree '()))

(module+ test
  (require rackunit)
  (define stream-tree
    (stream (stream (stream))
            (stream 1)
            (stream 2 3)
            (stream 4 (stream 5 6) (stream) (stream 7)
                    (stream (stream (stream))) (stream 8)
                    (stream 9))
            (stream)
            (stream 10 11 (stream 12) 13)
            14
            15
            (stream (stream (stream)))
            (stream (stream (stream) 16))
            (stream (stream (stream 17)))
            (stream (stream))))
  (define ssc (stream->stream-stack-cursor stream-tree))
  (check-equal?
   (stream->list ssc)
   (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17))

  ;; A test that should reflect the structure of parsing "4+4+4" with my parse-direct plus example.
  (define st2
    (stream 1
            (stream ;; +@1_l
             (stream ;; +@1_+, l=literal@1
              (stream ;; +@1_r
               '+13 ;; r=literal@3
               ;; r=+@3
               (stream ;; +@3_l
                (stream ;; +@3_+
                 (stream ;; +@3_r
                  '+1+35
                  )))
               )
              )
             ;; +@1
             (stream ;; +@1_+, l=+13
              (stream ;; +@1_r
               '++135
               )))))
  (check-equal?
   (stream->list (stream->stream-stack-cursor st2))
   (list 1 '+13 '+1+35 '++135))
  )

