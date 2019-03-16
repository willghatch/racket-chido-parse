#lang racket/base

(require
 "port-broker.rkt"
 racket/struct
 )

;; TODO - explanation from notes about the big picture of how this parsing library works

(struct parse-derivation
  ;; TODO - document from notes
  (result parser start-index end-index derivation-list)
  #:transparent)

(struct parser
  ;; TODO - document from notes
  (name prefix procedure)
  #:transparent)

(struct job
  ;; TODO - document from notes
  (id-number
   procedure/continuation
   extra-arguments
   result-index
   [dependencies #:mutable]
   )
  #:transparent)

(struct scheduler
  ;; TODO - document
  (job-list demand-stack)
  #:mutable
  #:transparent)
(define (make-scheduler '() '()))

#|
TODO - parser constructors
-- they should all have an optional #:name argument
- sequence parser
- alternative parser -- the alternative parser should turn the prefix into tries
- biased alternative parser -- PEG style -- try in order until just one succeeds
- literal parser -- takes a string
|#

#|
Result cache:
A multi-tier hash of port-broker->parser->extra-args-list->start-position->result-number->result.
The top level of the cache is a weak hash.

Scheduler cache:
A weak hash port-broker->scheduler.
|#
(define cache-empty-flag (gensym))
(define the-result-cache (make-weak-hasheq))
(define the-scheduler-cache (make-weak-hasheq))

(define (result-cache port-broker)
  (hash-ref! result-cache port-broker (hash)))

(define (hash-set+ h k1 k2 . args)
  (if (null? args)
      (hash-set h k1 k2)
      (hash-set h k1 (apply hash-set+
                            (hash-ref h k1 (λ (hash)))
                            k2
                            args))))
(define (hash-ref+ fail-thunk h k1 . args)
  (if (null? args)
      (hash-ref h k1 fail-thunk)
      (apply hash-ref+ fail-thunk (hash-ref h k1 fail-thunk) args)))

(define (set-result-cache! port-broker parser
                           extra-args start-position
                           result-number result)
  (define new-parser-cache
    (hash-set+ (hash-ref (result-cache port-broker) parser (λ (hash)))
               extra-args start-position result-number result))
  (hash-set! (result-cache port-broker) new-parser-cache))

(define (result-cache-ref port-broker parser
                          extra-args start-position
                          result-number)
  (hash-ref+ cache-empty-flag
             (result-cache port-broker)
             parser extra-args start-position result-number))



#|
cache thunk
- contains all the info to access the cache (or schedule an update to it)
- checks the cache, and if there is a result just return it
- else schedule a new job with the procedure and other cache info and also schedule a the current continuation as a job that depends on the new procedure.  Put the continuation job on the priority stack.  Also if there is a continuation mark denoting that this was done within another job, mark that anti-dependency on the new thunk job.
|#
(struct cache-thunk
  (port-broker parser extra-args start-position result-number)
  #:transparent)

(define (force-cache-thunk! ct)
  (define try-cache (apply result-cache-ref (struct->list ct)))
  (if (eq? try-cache cache-empty-flag)
      (let ([scheduler (hash-ref! the-scheduler-cache
                                  (cache-thunk-port-broker ct)
                                  (λ () (make-scheduler)))])
        ;; capture the continuation up to the chido-parse prompt
        ;; check if the parser is already scheduled!
        ;; schedule the parser (with no dependencies)
        ;; schedule the continuation with a dependency on the parser's job
        ;; if there is a continuation mark showing that this is a recursive call that is part of an existing job, update that job to have a dependency on this continuation.
        ;; add the continuation to the scheduler demand stack
        ;; call the scheduler!
        TODO
        )
      try-cache))

(define (run-scheduler s)
  ;; Check the top of the demand stack.
  ;; If all its dependencies are ready, call its continuation with results.
  ;; If it has unfinished dependencies, do a cycle-detecting BFS to find a dependency that has no dependencies.
  ;; If there is no dependency without a dependency, there is a cycle!  Call the demand continuation with a cycle failure result.
  ;; If there is a dependency that is ready, run it with a continuation mark saying what job it is.  When it returns, stuff the cache.  If it was a success, return it to the continuation! Else recur.
  ;; --- when stuffing the cache, it needs to hold the result AND a cache thunk that says how to get the next result -- it should likely call into a continuation...
  ;; --- if the result is an empty stream, treat it as an error.  If it is otherwise a stream, the way to get the next result is stream-next.... TODO - I need to take a break and come back to this.


  ;; TODO !! I don't think I need this continuation mark stuff -- if there is a recursive call I'll be building the demand stack.  I can check cycles using the demand stack itself rather than fussing with a bunch of continuation marks.
  TODO)


#|
parse-*/prefix is not a public API, but all the other parse API functions are built on top of it.
|#
;(define (parse-*/prefix TODO) TODO)
;(define (parse-*/whole TODO) TODO)
;(define (parse-1/prefix TODO) TODO)
;(define (parse-1/whole TODO) TODO)
