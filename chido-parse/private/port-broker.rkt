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

 port-broker-source-name

 port-broker-port-reset-position!
 )

(require
 racket/match
 data/gvector
 "ephemeron-cache.rkt"
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
  (if (or (< (port-broker-extent pb) char-offset) (eq? (port-broker-extent pb) 0))
      #f
      (gvector-ref (port-broker-contents pb)
                   (- char-offset (port-broker-offset pb)))))

(define (port-broker-char pb position)
  (define info (info-for pb position))
  (or (and info (vector-ref info info-char-offset))
      eof))
(define (port-broker-line/col pb position col?)
  (define info (info-for pb position))
  (define start (if col? column-start line-start))
  (define offset (if col? info-col-offset info-line-offset))
  (or (and info (vector-ref info offset))
      (and (eq? 0 (gvector-count (port-broker-contents pb))) start)
      (vector-ref (gvector-ref (port-broker-contents pb)
                               (sub1 (gvector-count (port-broker-contents pb))))
                  offset)))
(define (port-broker-line pb position)
  (port-broker-line/col pb position #f))
(define (port-broker-column pb position)
  (port-broker-line/col pb position #t))

(define (port-broker-substring? pb position string)
  (define len (string-length string))
  (for/and ([string-index (in-range len)]
            [pb-index (in-range position (+ position len))])
    (define info (info-for pb pb-index))
    ;; Apparently `eq?` doesn't work on non-ascii characters.  They must have an allocated wrapper around their integer value!
    (and info (equal? (vector-ref info info-char-offset)
                      (string-ref string string-index)))))

(define (port-broker-substring pb position length)
  (apply
   string
   (for/list ([i (in-range position (+ position length))])
     (define info (info-for pb i))
     (when (not info)
       (error 'port-broker-substring "substring goes beyond end of port"))
     (vector-ref info info-char-offset))))

(define (port-broker-source-name pb)
  (object-name (port-broker-port pb)))

(define (port-broker-port-reset-position! port new-pos)
  (when (< new-pos 1)
    (error 'port-broker-port-reset-position!
           "port reset to number less than 1: ~v" new-pos))
  (define reset-proc (hash-ref port->reset-proc port
                               (λ () (error 'port-broker-port-reset-position!
                                            "not given a valid port broker port: ~v"
                                            port))))
  (reset-proc new-pos))


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
                       (add1 (port-broker-extent pb))))
  (define byte-pos (if info
                       (vector-ref info info-byte-offset-offset)
                       (add1 (port-broker-peek-offset pb))))
  ;; For when byte reads request fewer bytes than necessary to complete a utf-8 char...
  (define incomplete-char-byte-pos #f)
  (define incomplete-char-byte-len #f)

  (define reset-procedure
    (λ (reset-to-char-pos)
      (define reset-pos-info (info-for pb reset-to-char-pos))
      (set! byte-pos (if reset-pos-info
                         (vector-ref reset-pos-info info-byte-offset-offset)
                         (add1 (port-broker-peek-offset pb))))
      (set! char-pos (if reset-pos-info
                         reset-to-char-pos
                         (add1 (port-broker-extent pb))))
      (set! incomplete-char-byte-pos #f)
      (set! incomplete-char-byte-len #f)))

  #|
  Read procedure.
  The result of the procedure is either the number of bytes read or eof.
  |#
  (define read-procedure
    (λ (mbytes-buffer)
      ;; Because ports are inherently byte-oriented, we have situations where we
      ;; read part of a character and leave the rest.
      (if incomplete-char-byte-pos
          (let* ([n-wanted (min (- incomplete-char-byte-len incomplete-char-byte-pos)
                                (bytes-length mbytes-buffer))]
                 [start-pos (+ byte-pos incomplete-char-byte-pos)]
                 [n-bytes (peek-bytes! mbytes-buffer
                                       start-pos
                                       (port-broker-port pb)
                                       0
                                       n-wanted)])
            (if (eq? incomplete-char-byte-len (+ n-bytes incomplete-char-byte-pos))
                (begin
                  (set! char-pos (add1 char-pos))
                  (set! byte-pos (+ byte-pos incomplete-char-byte-len))
                  (set! incomplete-char-byte-pos #f)
                  (set! incomplete-char-byte-len #f))
                (begin (set! incomplete-char-byte-pos (+ incomplete-char-byte-pos
                                                         n-bytes))))
            n-wanted)
          (let ([n-bytes-peeked
                 (peek-bytes! mbytes-buffer byte-pos (port-broker-port pb))])
            (if (eof-object? n-bytes-peeked)
                n-bytes-peeked
                (let ([n-chars-committed
                       ;; err-char is #f here, which allows this to sometimes return #f
                       (bytes-utf-8-length mbytes-buffer #f 0 n-bytes-peeked)])
                  (if n-chars-committed
                      (begin
                        (set! char-pos (+ n-chars-committed char-pos))
                        (set! byte-pos (+ n-bytes-peeked byte-pos))
                        (port-broker-extend-to pb char-pos)
                        n-bytes-peeked)
                      ;; If n-chars-committed is #f, then the peek isn't valid utf-8...
                      ;; So let's just read a char and make sure it's utf-8!
                      (let* ([str (peek-string 1
                                               byte-pos
                                               (port-broker-port pb))]
                             [buf-length (bytes-length mbytes-buffer)]
                             [sbytes (string->bytes/utf-8 str)]
                             [sbytes-length (bytes-length sbytes)])
                        ;; Extend a bit just to be sure I'm ahead.  TODO - is this necessary?
                        (port-broker-extend-to pb (+ 1 char-pos))
                        (if (< buf-length sbytes-length)
                            (let ()
                              ;; In this case we just have to read partial utf-8...
                              (set! incomplete-char-byte-pos n-bytes-peeked)
                              (set! incomplete-char-byte-len sbytes-length)
                              n-bytes-peeked)
                            (begin
                              (bytes-copy! mbytes-buffer 0 sbytes)
                              (set! char-pos (add1 char-pos))
                              (set! byte-pos (+ byte-pos sbytes-length))
                              sbytes-length))))))))))

  #|
  Peek procedure.
  If the event becomes ready the procedure must abort and return #f.
  Otherwise it should return the number of bytes peeked or an eof object.
  |#
  (define peek-procedure
    (λ (mbytes-buffer skip progress)
      (define n-bytes-peeked
        (peek-bytes! mbytes-buffer
                     (+ skip byte-pos
                        (or incomplete-char-byte-pos 0))
                     (port-broker-port pb)))
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
      (define contents (port-broker-contents pb))
      (cond [(and (not info) (eq? 0 (gvector-count contents)))
             (values line-start column-start char-start-index)]
            [(not info)
             (let* ([last-info (gvector-ref contents (sub1 (gvector-count contents)))])
               (values (vector-ref last-info info-line-offset)
                       (add1 (vector-ref last-info info-col-offset))
                       (+ (gvector-count contents) (port-broker-offset pb))))]
            [else (values (vector-ref info info-line-offset)
                          (vector-ref info info-col-offset)
                          char-pos)])))

  (define port-object
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
     char-pos
     ;; buffer-mode
     #f
     ))

  (hash-set! port->reset-proc port-object reset-procedure)
  ;; If port-count-lines! is not enabled, the custom location function is never called and everything is crazy and awful.
  (port-count-lines! port-object)

  port-object)



#;(define (close-port-broker pb)
  (close-input-port (port-broker-port pb)))



#|
Caching
A port should only have one port broker.
A wrapped port should be able to give a handle to the broker it is wrapping.
|#

;; broker -> original port (via ephemeron)
(define broker-cache
  (make-ephemeron-hasheq))

(define cache-port-broker
  (make-ephemeron-cache-lookup broker-cache make-port-broker))

;; wrapper port -> port broker
(define wrapper-cache
  (make-ephemeron-hasheq))

(define port->reset-proc
  (make-ephemeron-hasheq))

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
  (require "test-util-1.rkt")


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

  (define empty-pb (cache-port-broker (open-input-string "")))
  (check-equal? (port-broker-line empty-pb 0) 1)
  (check-equal? (port-broker-column empty-pb 0) 0)
  (check-equal? (port-broker-char empty-pb 1) eof)

  ;; Check that I con reset the position
  (port-broker-port-reset-position! wp1 1)
  (check-true (se? (wp1-read) r1))
  (define-values (after-1-line after-1-col after-1-pos) (port-next-location wp1))
  (check-true (se? (wp1-read) r2))
  (check-true (se? (wp1-read) r3))
  (port-broker-port-reset-position! wp1 after-1-pos)
  (define-values (after-1-line/again after-1-col/again after-1-pos/again)
    (port-next-location wp1))
  (check-equal? after-1-line after-1-line/again)
  (check-equal? after-1-col after-1-col/again)
  (check-equal? after-1-pos after-1-pos/again)
  (check-true (se? (wp1-read) r2))
  (define-values (after-2-line after-2-col after-2-pos) (port-next-location wp1))
  (check-not-equal? after-1-line after-2-line)
  (check-not-equal? after-1-col after-2-col)
  (check-not-equal? after-1-pos after-2-pos)
  (port-broker-port-reset-position! wp1 2)
  (define-values (line/2 col/2 pos/2) (port-next-location wp1))
  (port-broker-port-reset-position! wp1 4)
  (define-values (line/4 col/4 pos/4) (port-next-location wp1))
  (check-equal? pos/2 2)
  (check-equal? pos/4 4)

  (let ([pb-lozenge (cache-port-broker (open-input-string "a◊b"))])
    (check-true (port-broker-substring? pb-lozenge 2 "◊")))


)
