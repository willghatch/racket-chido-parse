#lang racket/base

(provide
 trie?
 trie-values
 empty-trie
 trie-ref
 trie-add

 trie-step
 trie-walk
 trie-leaf?
 trie-bare?
 trie-empty?
 )

(module+ test
  (require rackunit)
  )


(define (alist-ref dict key default)
  (define vp (assq key dict))
  (if vp (cdr vp) (do-default default)))

(define (alist-clear-key dict key)
  (remove key
          dict
          (λ (key p) (eq? key (car p)))))

(define (alist-set dict key val)
  (cons (cons key val)
        (alist-clear-key dict key)))

(module+ test
  (define alist1 (alist-set (alist-set (alist-set '() 'a 1)
                                       'b 2)
                            'c 3))
  (check-equal? (alist-ref alist1 'b #f)
                2)
  (check-equal? (alist-ref (alist-set alist1 'b 5)
                           'b
                           #f)
                5)
  )

(define dict-set alist-set)
(define dict-ref alist-ref)
(define dict-empty? null?)
(define empty-dict '())

;(define dict-set hash-set)
;(define dict-ref hash-ref)
;(define dict-empty? hash-empty?)
;(define empty-dict (hash))

#|
* My tries need to be keyed by strings (IE each level is keyed by characters) and values need to be listof any/c.
* I need to be able to tell whether a trie key (string) is a prefix of further keys.
|#


(struct trie (values dict)
  ;; dict is specifically an alist
  #:transparent)

(define (trie-leaf? t)
  (and (trie? t)
       (dict-empty? (trie-dict t))))

(define (trie-empty? x)
  (and (trie-leaf? x)
       (null? (trie-values x))))

(define (trie-bare? x)
  (and (trie? x)
       (null? (trie-values x))))

(define empty-trie (trie '() empty-dict))

;;; returns the values (list) from walking down the trie with `str` prefix
(define (trie-ref t str [default (λ () (error 'trie-ref "key not found: ~a" str))])
  (define ret (trie-walk t str not-found))
  (if (eq? not-found ret)
      (do-default default)
      (trie-values ret)))

;;; Returns the trie that is the result of taking one step in the trie.
(define (trie-step t c [default (λ () (error 'trie-step "key not found: ~a" c))])
  (dict-ref (trie-dict t) c default))

;;; Returns the trie that is the result of walking down `str`
(define (trie-walk t str [default (λ () (error 'trie-walk "key not found: ~a" str))])
  (let loop ([t t]
             [keys (string->list str)])
    (define ret (trie-step t (car keys) not-found))
    (if (eq? not-found ret)
        (do-default default)
        (if (null? (cdr keys))
            ret
            (loop ret (cdr keys))))))


;;; adds a value to the value list at prefix `str`
(define (trie-add t str v)
  (define (rec t keys v)
    (cond [(null? keys) (trie (cons v (trie-values t)) (trie-dict t))]
          [(trie-step t (car keys) #f)
           =>
           (λ (nt) (trie (trie-values t)
                         (dict-set (trie-dict t)
                                   (car keys)
                                   (rec nt (cdr keys) v))))]
          [else (rec (trie (trie-values t)
                           (dict-set (trie-dict t)
                                     (car keys)
                                     empty-trie))
                     keys
                     v)]))
  (rec t (string->list str) v))

;;; clears values list at prefix `str`
#;(define (trie-clear t str)
    TODO)

;;; Helper for defaults a la hash-ref
(define (do-default x)
  (if (procedure? x)
      (x)
      x))
(define not-found (gensym))


(module+ test

  (define t1
    (trie-add
     (trie-add
      (trie-add
       (trie-add empty-trie "abc" 'abc)
       "ab"
       'ab)
      "abeef"
      'abeef)
     "abc"
     'abc2))

  (check-equal? (trie-ref t1 "abc")
                '(abc2 abc))
  (check-equal? (trie-ref t1 "abeef")
                '(abeef))
  (check-equal? (trie-ref t1 "a")
                '())

  (check-equal? (trie-ref (trie-walk t1 "ab") "eef")
                '(abeef))

  (check-equal? (trie-ref t1 "abcde" 'def)
                'def)
  (check-true (trie-leaf? (trie-walk t1 "abeef")))
  (check-exn exn? (λ () (trie-ref t1 "abeefa")))
  )
