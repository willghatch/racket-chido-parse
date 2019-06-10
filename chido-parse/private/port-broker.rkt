#lang racket/base

(provide
 (rename-out
  [cache-port-broker port-broker]
  )
 port-broker->port
 port->port-broker
 ;close-port-broker
 ;port-broker-commit-bytes

 port-broker?
 port-broker-char
 port-broker-line
 port-broker-column
 port-broker-substring?
 port-broker-substring
 )

(require
 racket/match
 data/gvector
 "util.rkt"
 )

(define info-byte-offset-offset 0)
(define info-char-offset 1)
(define info-line-offset 2)
(define info-col-offset 3)

(struct port-broker
  (port
   ;; a gvector of (vector-immutable byte-offset char line col) indexed by position
   contents
   ;; offset for content -- content[0] refers to the offset position
   ;; offset will always be at least 1.
   offset
   ;; the number of BYTES offset for the next peek.
   [peek-offset #:mutable]
   [last-line #:mutable]
   [last-column #:mutable]
   [eof-ed? #:mutable]
   )
  #:transparent)

(define byte-start-index 0)
(define char-start-index 1)

;; Racket syntax objects use 1 as the first line and 0 as the first column.
(define line-start 1)
(define column-start 0)

(define (make-port-broker port)
  (define-values (line* col* pos) (port-next-location port))
  (define line (or line* line-start))
  (define col (or col* column-start))
  (define contents (make-gvector #:capacity 500))
  (port-broker port
               contents
               pos
               0
               line
               col
               #f))


(define (port-broker-extend-1 pb)
  ;; Returns true if no eof has been reached.
  (define char (peek-char (port-broker-port pb)
                          (port-broker-peek-offset pb)))
  (if (eof-object? char)
      (begin
        (set-port-broker-eof-ed?! pb #t)
        #f)
      (let* ([char-len (char-utf-8-length char)]
             [new-peek-offset (+ char-len (port-broker-peek-offset pb))]
             [newline? (eq? #\newline char)]
             [new-line (if newline?
                           (add1 (port-broker-last-line pb))
                           (port-broker-last-line pb))]
             [new-column (if newline?
                             0
                             (add1 (port-broker-last-column pb)))])
        (gvector-add! (port-broker-contents pb)
                      (vector-immutable
                       (port-broker-peek-offset pb)
                       char
                       (port-broker-last-line pb)
                       (port-broker-last-column pb)))
        (set-port-broker-peek-offset! pb new-peek-offset)
        (set-port-broker-last-line! pb new-line)
        (set-port-broker-last-column! pb new-column)
        #t)))

(define (port-broker-extend-all pb)
  (if (port-broker-eof-ed? pb)
      (void)
      (let loop ()
        (when (port-broker-extend-1 pb)
          (loop)))))

(define (port-broker-extend-n pb n)
  (if (port-broker-eof-ed? pb)
      #f
      (let loop ([n n])
        (if (< 0 n)
            (and (port-broker-extend-1 pb) (loop (sub1 n)))
            #t))))

(define (port-broker-extent pb)
  (sub1
   (+ (port-broker-offset pb)
      (gvector-count (port-broker-contents pb)))))

(define (port-broker-extend-to pb char-offset)
  (port-broker-extend-n pb (add1 (- char-offset (port-broker-extent pb)))))


(define (info-for pb char-offset)
  (when (not (port-broker-eof-ed? pb))
    (port-broker-extend-to pb char-offset))
  (if (< (port-broker-extent pb) char-offset)
      #f
      (gvector-ref (port-broker-contents pb)
                   (- char-offset (port-broker-offset pb)))))

(define (port-broker-char pb position)
  (define info (info-for pb position))
  (or (and info (vector-ref info info-char-offset))
      eof))
(define (port-broker-line pb position)
  (define info (info-for pb position))
  (or (and info (vector-ref info info-line-offset))
      (vector-ref (gvector-ref (port-broker-contents pb)
                               (sub1 (gvector-count (port-broker-contents pb))))
                  info-line-offset)))
(define (port-broker-column pb position)
  (define info (info-for pb position))
  (or (and info (vector-ref info info-line-offset))
      (add1
       (vector-ref (gvector-ref (port-broker-contents pb)
                                (sub1 (gvector-count (port-broker-contents pb))))
                   info-char-offset))))

(define (port-broker-substring? pb position string)
  (define len (string-length string))
  (for/and ([string-index (in-range len)]
            [pb-index (in-range position (+ position len))])
    (define info (info-for pb pb-index))
    (and info (eq? (vector-ref info info-char-offset)
                   (string-ref string string-index)))))

(define (port-broker-substring pb position length)
  (apply
   string
   (for/list ([i (in-range position (+ position length))])
     (define info (info-for pb i))
     (when (not info)
       (error 'port-broker-substring "substring goes beyond end of port"))
     (vector-ref info info-char-offset))))


(define (port-broker->wrapped-port pb char-offset)
  #|
  The wrapper should act like a real port that can be `read` and `peeked`,
  despite the fact that it will only ever `peek` the actual input port.
  So we need to keep track of the read head of this pseudo-port.
  |#
  (when (< char-offset 1)
    (error 'port-broker->wrapped-port "minimum char-offset for ports is 1."))
  (define info (info-for pb char-offset))
  (define char-pos (if info
                       char-offset
                       (port-broker-extent pb)))
  (define byte-pos (if info
                       (vector-ref info 0)
                       (port-broker-peek-offset pb)))

  #|
  Read procedure.
  The result of the procedure is either the number of bytes read or eof.
  |#
  (define read-procedure
    (λ (mbytes-buffer)
      (define n-bytes-peeked
        (peek-bytes! mbytes-buffer byte-pos (port-broker-port pb)))
      (when (not (eof-object? n-bytes-peeked))
        (define n-chars-committed
          ;; TODO - err-char is #f here, which allows this to sometimes return #f
          (bytes-utf-8-length mbytes-buffer #f 0 n-bytes-peeked))
        (set! char-pos (+ n-chars-committed char-pos))
        (set! byte-pos (+ n-bytes-peeked byte-pos))
        (port-broker-extend-to pb char-pos))
      n-bytes-peeked))

  #|
  Peek procedure.
  If the event becomes ready the procedure must abort and return #f.
  Otherwise it should return the number of bytes peeked or an eof object.
  |#
  (define peek-procedure
    (λ (mbytes-buffer skip progress)
      (define n-bytes-peeked
        (peek-bytes! mbytes-buffer (+ skip byte-pos) (port-broker-port pb)))
      n-bytes-peeked))

  (define commit-procedure
    (λ (amount-to-commit progress-event done-event)
      ;; Technically this should listen to the events and maybe not do stuff,
      ;; but that is for crazy concurrency support, which I won't support.
      (define bytes (peek-bytes amount-to-commit byte-pos (port-broker-port pb)))
      ;; TODO - this length procedure can return #f if it's not encoded well...
      (define char-len (bytes-utf-8-length bytes))
      (set! char-pos (+ char-len char-pos))
      (set! byte-pos (+ byte-pos amount-to-commit))))

  (define get-location
    (λ ()
      (define info (info-for pb char-pos))
      (if (not info)
          (let* ([contents (port-broker-contents pb)]
                 [last-info (gvector-ref contents (sub1 (gvector-count contents)))])
            (values (vector-ref last-info info-line-offset)
                    (add1 (vector-ref last-info info-col-offset))
                    (+ -1 (gvector-count contents) (port-broker-offset pb))))
          (values (vector-ref info info-line-offset)
                  (vector-ref info info-col-offset)
                  char-pos))))

  (make-input-port
   (object-name (port-broker-port pb))
   read-procedure
   peek-procedure
   ;; close procedure
   (λ () (void))
   ;; get-progress-evt
   (λ () always-evt)
   commit-procedure
   get-location
   ;; count-lines!
   (λ () (void))
   ;; init-position
   char-offset
   ;; buffer-mode
   #f
   ))



#;(define (close-port-broker pb)
  (close-input-port (port-broker-port pb)))



#|
Caching
A port should only have one port broker.
A wrapped port should be able to give a handle to the broker it is wrapping.
|#

;; broker -> original port (via ephemeron)
(define broker-cache
  (make-weak-hasheq))

(define cache-port-broker
  (make-ephemeron-cache-lookup broker-cache make-port-broker))

;; wrapper port -> port broker
(define wrapper-cache
  (make-weak-hasheq))

(define (port-broker->wrapped-port/cached pb offset)
  (define p (port-broker->wrapped-port pb offset))
  (hash-set! wrapper-cache p pb)
  p)

(define (port-broker->port pb offset)
  (port-broker->wrapped-port/cached pb offset))


(define (port->port-broker p)
  (hash-ref wrapper-cache p #f))



(module+ test
  (require rackunit)
  (define (se? a b)
    ;; syntax-equal? for datum and position.
    ;; Ignores syntax properties.
    (or
     (and (match (list a b)
            [(list (list ai ...) (list bi ...))
             (andmap se? ai bi)]
            [(list (? syntax?) b) #f]
            [else (equal? a b)]))
     (and (syntax? a)
          (syntax? b)
          (se? (syntax-e a) (syntax-e b))
          (equal? (syntax-source a) (syntax-source b))
          (equal? (syntax-line a) (syntax-line b))
          (equal? (syntax-column a) (syntax-column b))
          (equal? (syntax-position a) (syntax-position b))
          (equal? (syntax-span a) (syntax-span b)))
     (begin
       (if (and (syntax? a) (syntax? b))
           (eprintf "different: ~s while expected ~s\n"
                    (list a (syntax->datum a) (syntax-source a)
                          (syntax-line a) (syntax-column a)
                          (syntax-position a) (syntax-span a))
                    (list b (syntax->datum b) (syntax-source b)
                          (syntax-line b) (syntax-column b)
                          (syntax-position b) (syntax-span b)))
           (eprintf "different: ~s while expected ~s\n" a b))
       #f)))


  (define test-str
    "(this is a test)
(more testing
      test test
      test)
(test)")
;;;


  (define p-normal (open-input-string test-str 'test-str))
  (port-count-lines! p-normal)
  (define oread (λ () (parameterize ([current-input-port p-normal])
                        (read-syntax))))
  (define r1 (oread))
  (define r2 (oread))
  (define r3 (oread))
  (define r4 (oread))

  (define p-to-wrap (open-input-string test-str 'test-str))
  ;(define pb1 (make-port-broker p-to-wrap))
  (define pb1 (cache-port-broker p-to-wrap))

  (define wp1 (port-broker->wrapped-port pb1 1))
  (port-count-lines! wp1)
  (define wp1-read (λ () (parameterize ([current-input-port wp1])
                           (read-syntax))))
  (define wp1-r1 (wp1-read))
  (define wp1-r2 (wp1-read))
  (define wp1-r3 (wp1-read))
  (define wp1-r4 (wp1-read))
  (check-true (se? wp1-r1 r1))
  (check-true (se? wp1-r2 r2))
  (check-true (se? wp1-r3 r3))
  (check-true (se? wp1-r4 r4))

  ;; Now check that I can still read with a second port from the same broker.
  (define wp2 (port-broker->wrapped-port pb1 1))
  (port-count-lines! wp2)
  (define wp2-read (λ () (parameterize ([current-input-port wp2])
                          (read-syntax))))
  (check-true (se? (wp2-read) r1))
  (check-true (se? (wp2-read) r2))
  (check-true (se? (wp2-read) r3))
  (check-true (se? (wp2-read) r4))

  ;; Check that I can start in the middle
  (define wp3 (port-broker->wrapped-port pb1 17))
  (port-count-lines! wp3)
  (define wp3-read (λ () (parameterize ([current-input-port wp3])
                           (read-syntax))))
  (check-true (se? (wp3-read) r2))

  (check-true (port-broker-substring? pb1 3 "his"))
  (check-equal? (port-broker-substring pb1 3 3)
                "his")

  #|
  TODO - port-broker-commit-bytes is broken.  The following test fails.
  |#
  ;(port-broker-commit-bytes pb1 15)
  ;(check-true (se? (wp3-read) r3))



)
