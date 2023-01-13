(module reader syntax/module-reader
  #:language (lambda (p) (read-syntax (object-name p) p))
  #:read-syntax read-syntax-proc
  #:read (λ (port) (syntax->datum (read-syntax-proc "name" port)))
  #:whole-body-readers? #t
  #:info (λ (key defval default)
           (case key
             [(color-lexer)
              (dynamic-require 'syntax-color/default-lexer
                               'default-lexer)]
             [(drracket:submit-predicate)
              (dynamic-require 'rash/private/drracket-submit-predicate
                               'submit-predicate)]
             [else (default key defval)]))
  (require (submod "../../readtable-parser.rkt" an-s-exp-readtable read-syntax-proc))
  #;(define read-syntax-proc
    (λ (name port)
      (define out
        (let loop ([results '()])
          (define r (read-syntax name port))
          (if (eof-object? r)
              (reverse results)
              (loop (cons r results)))))
      (eprintf "out: ~a\n" out)
      (define out2
        (datum->syntax #f '((printf "hi mars\n") (printf "testing foo\n"))))
      out2
      ))
  )
