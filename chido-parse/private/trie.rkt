#lang racket/base

(provide
 empty-trie
 trie-insert
 trie-insert*
 trie-ref
 )

;; This module is basically a more convenient interface of pfds/trie.  My keys are always strings (rather than lists of arbitrary things).
;; Also pfds/trie has an interface that is inconsistent with standard data structure get/set functions (dict-ref, etc), and even internally inconsistent (insert vs bind argument order).


(require
 pfds/trie
 racket/match
 )


(define empty-trie (tries '() '()))

(define (trie-insert t key-string v)
  (bind (string->list key-string) v t))

(define (list->parallel-lists args)
  (match args
    [(list k v) (values (list k) (list v))]
    [(list k v rest ...)
     (define-values (ks vs) (list->parallel-lists rest))
     (values (cons k ks) (cons v vs))]
    ;; Because this is only used in trie-insert*...
    [(list k) (error 'trie-insert* "uneven argument list")]))

(define (trie-insert* t . args)
  (define-values (ks vs) (list->parallel-lists args))
  (insert vs (map string->list ks) t))

(define (trie-ref t key-string
                  [default-thunk (λ () (error 'trie-ref "key not found"))])
  (with-handlers ([(λ(e)#t)(λ(e) (if (procedure? default-thunk)
                                     (default-thunk)
                                     default-thunk))])
    (lookup (string->list key-string) t)))


(module+ test
  (require rackunit)
  (define t1 (trie-insert (trie-insert empty-trie "a" 5)
                          "abc"
                          99))
  (define t2 (trie-insert* empty-trie "a" 5 "abc" 99))
  (check-equal? (trie-ref t1 "a")
                5)
  (check-equal? (trie-ref t1 "a")
                (trie-ref t2 "a"))
  (check-equal? (trie-ref t1 "abc")
                (trie-ref t2 "abc"))
  (check-equal? (trie-ref t1 "foo" #f)
                #f)
  (check-equal? (trie-ref t1 "foo" (λ()42))
                42)
  (check-exn exn? (λ () (trie-ref t1 "foo")))
  )
