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

(struct parse-error
  ;; TODO
  ()
  #:transparent)

(struct parser
  ;; TODO - document from notes
  (name prefix procedure)
  #:transparent)

(struct alt-parser
  ;; TODO - use a prefix trie for the parsers
  (name parsers)
  #:transparent)

(define (new-alt-parser #:name [name #f] . parsers)
  (alt-parser name parsers))

#|
Schedulers keep track of parse work that needs to be done.
The job-list can contain:
* parser-jobs, which contain all the info to run a parser and cache its results
* scheduled-continuations, which have an outer job that they represent (or #f for the original outside caller) and a single dependency.
* alt-workers, which are special workers for alt-parsers.  Alt-workers are spawned instead of normal continuations for alt-parsers.
|#
(struct scheduler
  ;; TODO - document
  (job-list demand-stack)
  #:mutable
  #:transparent)
(define (make-scheduler '() '()))

(struct parser-job
  ;; TODO - document from notes
  (id-number
   parser
   extra-arguments
   result-index
   [continuation/worker #:mutable]
   [dependents #:mutable]
   )
  #:transparent)

(struct alt-worker
  (job remaining-jobs failures successful?)
  #:mutable #:transparent)

(struct scheduled-continuation
  (job k dependency [ready? #:mutable])
  #:transparent)

(define job-id-counter 0)
(define (fresh-id-number!)
  (set! job-id-counter (add1 job-id-counter))
  job-id-counter)
(define (make-bare-continuation-job k deps)
  (job (fresh-id-number!) #f #f #f k deps))
(define (job->k-job j k deps)
  (struct-copy job j
               [continuation k]
               [dependencies deps]))

(define (cache-thunk->job ct)
  (job (fresh-id-number!) (cache-thunk-parser ct) (cache-thunk-extra-args ct)
       (cache-thunk-start-position ct) (cache-thunk-result-number ct)
       '() '()))




#|
TODO - parser constructors
-- they should all have an optional #:name argument
- sequence parser
- alternative parser -- the alternative parser should turn the prefix into tries
- biased alternative parser -- PEG style -- try in order until just one succeeds
- repetition parser
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

(define chido-parse-prompt (make-continuation-prompt-tag 'chido-parse-prompt))
(define chido-parse-k-mark (make-continuation-mark-key 'chido-parse-k-mark))

(define (force-cache-thunk! ct)
  TODO
  ;; TODO - this is all a mess!!  I wrote most of this before I made a bunch of changes to all the relevant structs and rewrote the run-scheduler function.

  (define try-cache (apply result-cache-ref (struct->list ct)))
  (if (eq? try-cache cache-empty-flag)
      (let ([scheduler (hash-ref! the-scheduler-cache
                                  (cache-thunk-port-broker ct)
                                  (λ () (make-scheduler)))])
        (call-with-current-continuation
         (λ (k)
           (define k-marks (continuation-marks k))
           (define outer-job-marks (continuation-mark-set->list k-marks
                                                                chido-parse-k-mark
                                                                chido-parse-prompt))
           (define parent-job (match outer-job-marks
                                ['() #f]
                                [(list j) j]
                                [else (error
                                       'chido-parse
                                       "internal error -- bad continuation marks")]))
           (define parser-job (cache-thunk->job ct))
           (schedule-job! scheduler parser-job)
           (define k-job (if parent-job
                             (job->k-job parent-job k)
                             (make-bare-continuation-job k
                                                         (list parser-job))))
           (when parent-job
             (set-job-dependencies! parent-job
                                    (cons k-job (job-dependencies parent-job))))
           (schedule-job! scheduler k-job)
           (demand-job! scheduler k-job)
           (run-scheduler scheduler))
         chido-parse-prompt))
      try-cache))

(define (find-work s job-list)
  ;; Returns an actionable job or #f.
  (define demands (scheduler-demand-stack s))
  (let loop ([jobs job-list]
             [blocked '()])
    (if (null? jobs)
        #f
        (let ([j (car jobs)])
          (cond [(member j blocked) (loop (cdr jobs) blocked)]
                [(member (parser-job-continuation/worker j) demands)
                 (loop (cdr jobs) blocked)]
                ;; if it has a continuation/worker, add dependencies to jobs
                [else (match (parser-job-continuation/worker j)
                        [(scheduled-continuation job k dependency ready?)
                         (cond [ready? j]
                               [else (loop (cons dependency (cdr jobs))
                                           (cons j blocked))])]
                        [(alt-worker job remaining-jobs failures successful?)
                         (loop (append remaining-jobs jobs)
                               (cons j blocked))]
                        [#f j])])))))

(define (run-scheduler s)
  (define demanded (car (scheduler-demand-stack s)))
  (define (pop-demand!)
    (set-scheduler-demand-stack! s (cdr (scheduler-demand-stack s))))
  (match demanded
    [(scheduled-continuation job k dependency ready?)
     (define (return! ret)
       (pop-demand!)
       (k result))
     (if ready?
         (let ([result (lookup-job-result dependency)])
           (return! result))
         (let ([actionable-job (find-work s (list dependency))])
           (if (not actionable-job)
               (return (make-cycle-failure job))
               (run-actionable-job actionable-job))))]
    [(alt-worker job remaining-jobs failures successful?)
     (cond [(and (null? remaining-jobs) successful?)
            ;; * cache the result for the job as an empty stream
            ;; * mark all of its dependents as ready
            ;; * pop the demand stack
            ;; * recur into run-scheduler
            TODO]
           [(null? remaining-jobs)
            ;; * make a failure result that encapsulates the failures list
            ;; * cache the result of the job as a failure
            ;; * mark all of its dependents as ready
            ;; * pop the demand stack
            ;; * recur into run-scheduler
            TODO]
           [else (let ([actionable-job (find-work s remaining-jobs)])
                   (if (not actionable-job)
                       (begin
                         (make-cycle-failure job)
                         (pop-demand!)
                         (run-scheduler s))
                       (run-actionable-job actionable-job)))])]))

(define (make-cycle-failure job)
  ;; * cache a cycle failure for the result of the job
  ;; * mark its dependents as ready
  TODO)

(define (run-actionable-job job)
  ;; TODO - same as when running actionable job for scheduled cont!
  ;; Run the job with a continuation mark saying what job we are in (so we can associate the scheduled-continuation with this job if/when it happens).
  ;; When the job returns:
  ;; * cache the result
  ;; * mark all of its dependents as ready
  ;; * recur into run-scheduler
  TODO
  ;; run with mark
  (with-continuation-mark chido-parse-k-mark job this-job-proc)
  ;; --- when stuffing the cache, it needs to hold the result AND a cache thunk that says how to get the next result -- it should likely call into a continuation...  Or maybe the next job should be scheduled with a scheduled-continuation already...
  ;; --- if the result is an empty stream, treat it as an error.  If it is otherwise a stream, the way to get the next result is stream-next.... TODO - I need to take a break and come back to this.
  )





#|
parse-*/prefix is not a public API, but all the other parse API functions are built on top of it.
|#
;(define (parse-*/prefix TODO) TODO)
;(define (parse-*/whole TODO) TODO)
;(define (parse-1/prefix TODO) TODO)
;(define (parse-1/whole TODO) TODO)



