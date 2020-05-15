#lang racket/base

;; TODO - this is for the demo of dynamically loading rash code.
;;        Really, I should re-implement linea in chido-parse.
;;        But for expediency I'll just wrap it like this.

(require chido-parse linea/read)
(provide dynamic-parser)
(define dynamic-parser
  (proc-parser
   (λ (port)
     (linea-read-syntax (object-name port) port))))
