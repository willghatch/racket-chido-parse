#lang racket/base

(provide make-ephemeron-cache-lookup)

(define (make-ephemeron-cache-lookup cache fresh-value-constructor)
  (λ (key)
    (ephemeron-value (hash-ref cache key
                               (λ () (let* ([v (fresh-value-constructor key)]
                                            [e (make-ephemeron key v)])
                                       (hash-set! cache key e)
                                       e))))))

