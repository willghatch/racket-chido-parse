#lang racket/base

(provide make-ephemeron-cache-lookup)

(define (make-ephemeron-cache-lookup cache fresh-value-constructor)
  (λ (key)
    (or (hash-ref cache key #f)
        (let* ([v (fresh-value-constructor key)])
          (hash-set! cache key v)
          v))))
