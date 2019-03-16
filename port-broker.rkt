#lang racket/base

(provide
 (rename-out
  [cache-port-broker port-broker]
  [port-broker->wrapped-port/cached port-broker->port]
  )
 port->port-broker
 close-port-broker
 ;port-broker-commit-bytes
 )

(require
 racket/match
 data/gvector
 "util.rkt"
 )

(struct port-broker
  #|
  The newlines field is a gvector containing (cons byte-offset char-offset).
  The inner port is peeked a character at a time, and each time the peeked character is a newline a cell is added to the newlines field.
  |#
  (port
   newlines
   [last-byte-offset #:mutable]
   [last-char-offset #:mutable]
   [last-char-byte-length #:mutable]
   [current-byte-offset #:mutable]
   [current-char-offset #:mutable]
   [current-char-column #:mutable]
   )
  #:transparent)

(define byte-start-index 0)
(define char-start-index 1)
(define column-start-index 0)

(define (make-port-broker port)
  (when (not (equal? 0 (file-position port)))
    (error 'make-port-broker "Port brokers must start with a fresh port."))
  ;; char offset starts at 1 (for syntax position counting), while
  ;; byte offset starts at 0.
  (port-broker port
               (gvector)
               byte-start-index
               char-start-index
               #f
               byte-start-index
               char-start-index
               column-start-index))

(define (port-broker-commit-bytes pb num-bytes)
  (define port (port-broker-port pb))
  (define peeked (peek-bytes num-bytes 0 port))
  (define new-byte-offset (+ (port-broker-current-byte-offset pb) num-bytes))
  (define-values (line col pos)
    (port-broker-get-location pb new-byte-offset))
  (set-port-broker-current-char-column! pb col)
  (set-port-broker-current-byte-offset! pb new-byte-offset)
  (set-port-broker-current-char-offset! pb (+ (port-broker-current-char-offset pb)
                                              (bytes-utf-8-length peeked)))
  (port-commit-peeked num-bytes (port-progress-evt port) always-evt port)
  (void))

(define (port-broker-peek-bytes mbytes-buffer offset pb)
  ;; offset is the byte offset from the port START, not the current position.
  (define (add-newline! c byte-offset char-offset)
    (when (equal? c #\newline)
      (gvector-add! (port-broker-newlines pb)
                    (cons byte-offset char-offset))))
  (define p (port-broker-port pb))
  (when (not (port-broker-last-char-byte-length pb))
    ;; If the length has not yet been set, then this is the first peek.
    (define c (peek-char p 0))
    (when (char? c)
      (set-port-broker-last-char-byte-length! pb (char-utf-8-length c))))
  (define real-byte-position (file-position p))
  (define real-port-offset (- offset real-byte-position))
  (when (< real-port-offset 0)
    ;; TODO - better error message.
    (error
     'chido-parse
     "Trying to peek data from a port at a position before the current port position."))
  (define len (bytes-length mbytes-buffer))
  (define new-offset-wanted (+ len offset))
  (when (< (port-broker-last-byte-offset pb) new-offset-wanted)
    (let/cc return
      (let loop ()
        (when (>= (port-broker-last-byte-offset pb) new-offset-wanted)
          (return #f))
        (define new-b-offset (+ (port-broker-last-byte-offset pb)
                                (port-broker-last-char-byte-length pb)))
        (define new-c (peek-char p new-b-offset))
        (when (eof-object? new-c)
          (return #f))
        (define new-c-len (char-utf-8-length new-c))
        (set-port-broker-last-byte-offset! pb new-b-offset)
        (define new-c-offset (add1 (port-broker-last-char-offset pb)))
        (set-port-broker-last-char-offset! pb new-c-offset)
        (set-port-broker-last-char-byte-length! pb new-c-len)
        (add-newline! new-c new-b-offset new-c-offset)
        (loop))))
  (peek-bytes! mbytes-buffer real-port-offset p))

(define (newline-index->line-number index)
  ;; Index 0 is the first NEWLINE, and the line before it is line 1.
  (+ 2 index))

(define (index-binary-search newlines offset)
  ;; Return the index number for the newline to the LEFT of the offset.
  (define (rec low high)
    (if (equal? low high)
        low
        (let* ([center (+ low (ceiling (/ (- high low)
                                          2)))]
               [center-offset (car (gvector-ref newlines center))])
          (if (<= offset center-offset)
              (rec low (sub1 center))
              (rec center high)))))
  (cond [(equal? 0 (gvector-count newlines)) -1]
        [(<= offset (car (gvector-ref newlines 0))) -1]
        [else (rec 0 (sub1 (gvector-count newlines)))]))

(define (port-broker-current-line-number pb)
  (define newlines (port-broker-newlines pb))
  (define n-lines (gvector-count newlines))
  (newline-index->line-number
   (index-binary-search newlines (port-broker-current-byte-offset pb))))

(define (port-broker-get-location/current-line pb offset)
  (define current-byte-pos (port-broker-current-byte-offset pb))
  (define current-char-pos (port-broker-current-char-offset pb))
  (define current-char-column (port-broker-current-char-column pb))
  (define amount (- offset current-byte-pos))
  (define peeked (peek-bytes amount 0 (port-broker-port pb)))
  (if (eof-object? peeked)
      (values (port-broker-current-line-number pb)
              current-char-column
              current-char-pos)
      (let ([char-len (bytes-utf-8-length peeked)])
        (values (port-broker-current-line-number pb)
                (+ char-len current-char-column)
                (+ char-len current-char-pos)))))

(define (port-broker-get-location pb offset)
  ;; offset is the byte offset from the port START, not the current position.
  (define newlines (port-broker-newlines pb))
  (define n-lines (gvector-count newlines))
  (let ([n-index (index-binary-search newlines offset)])
    (match (if (< n-index 0)
               (cons byte-start-index char-start-index)
               (gvector-ref newlines n-index))
      [(cons nl-byte-offset nl-char-offset)
       (when (< offset nl-byte-offset)
         (error 'chido-parse "internal error when getting location"))
       (if (<= nl-byte-offset (port-broker-current-byte-offset pb))
           (port-broker-get-location/current-line pb offset)
           (let* ([buffer-size (- offset nl-byte-offset)]
                  [buffer (make-bytes buffer-size)]
                  [n-peeked (port-broker-peek-bytes buffer
                                                    buffer-size
                                                    pb)])
             (when (not (equal? buffer-size n-peeked))
               (error 'chido-parse "internal error getting location"))
             (define char-len (bytes-utf-8-length buffer))
             (values (newline-index->line-number n-index)
                     ;; the char-len includes the newline character
                     (sub1 char-len)
                     (+ nl-char-offset char-len))))])))

(define (port-broker->wrapped-port pb byte-offset)
  #|
  The wrapper should act like a real port that can be `read` and `peeked`,
  despite the fact that it will only ever `peek` the actual input port.
  So we need to keep track of the read head of this pseudo-port.
  |#
  (define committed-bytes 0)

  (make-input-port
   (object-name (port-broker-port pb))

   #|
   Read procedure.
   The result of the procedure is either the number of bytes read or eof.
   |#
   (λ (mbytes-buffer)
     (define bytes-peeked
       (port-broker-peek-bytes mbytes-buffer
                               (+ byte-offset committed-bytes)
                               pb))
     (when (not (eof-object? bytes-peeked))
       (set! committed-bytes (+ committed-bytes bytes-peeked)))
     bytes-peeked)

   #|
   Peek procedure.
   If the event becomes ready the procedure must abort and return #f.
   Otherwise it should return the number of bytes peeked or an eof object.
   |#
   (λ (mbytes-buffer skip progress)
     (port-broker-peek-bytes mbytes-buffer
                             (+ byte-offset committed-bytes skip)
                             pb))

   ;; close procedure
   (λ () (void))

   ;; get-progress-evt
   (λ () always-evt)

   ;; commit
   (λ (amount-to-commit progress-event done-event)
     ;; Technically this should listen to the events and maybe not do stuff,
     ;; but that is for crazy concurrency support, which I won't support.
     (set! committed-bytes (+ committed-bytes amount-to-commit)))

   ;; get-location
   (λ ()
     (port-broker-get-location pb (+ byte-offset committed-bytes)))

   ;; count-lines!
   (λ () (void))

   ;; init-position
   (let-values ([(line col pos) (port-broker-get-location pb byte-offset)])
     pos)

   ;; buffer-mode
   #f
   ))

(define (close-port-broker pb)
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

(define (port-broker->wrapped-port/cached pb)
  (define p (port-broker->wrapped-port pb))
  (hash-set! wrapper-cache p pb)
  p)

(define (port->port-broker p)
  (hash-ref wrapper-cache p (λ () (error
                                   'port->port-broker
                                   "not a port created by port-broker->port"))))



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
  (define pb1 (make-port-broker p-to-wrap))

  (define wp1 (port-broker->wrapped-port pb1 0))
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
  (define wp2 (port-broker->wrapped-port pb1 0))
  (port-count-lines! wp2)
  (define wp2-read (λ () (parameterize ([current-input-port wp2])
                          (read-syntax))))
  (check-true (se? (wp2-read) r1))
  (check-true (se? (wp2-read) r2))
  (check-true (se? (wp2-read) r3))
  (check-true (se? (wp2-read) r4))

  ;; Check that I can start in the middle
  (define wp3 (port-broker->wrapped-port pb1 16))
  (port-count-lines! wp3)
  (define wp3-read (λ () (parameterize ([current-input-port wp3])
                           (read-syntax))))
  (check-true (se? (wp3-read) r2))

  #|
  TODO - port-broker-commit-bytes is broken.  The following test fails.
  |#
  ;(port-broker-commit-bytes pb1 15)
  ;(check-true (se? (wp3-read) r3))

)
