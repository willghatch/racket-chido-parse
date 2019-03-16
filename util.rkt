#lang racket/base

(provide make-ephemeron-cache-lookup)

(define (make-ephemeron-cache-lookup cache fresh-value-constructor)
  (λ (key)
    (if (hash-has-key? cache key)
        (ephemeron-value (hash-ref cache key))
        (let ([v (fresh-value-constructor key)])
          (hash-set! cache key (make-ephemeron key v))
          v))))

