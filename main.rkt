#lang racket/base

(require
 "port-broker.rkt"
 "util.rkt"
 racket/struct
 )

;; TODO - explanation from notes about the big picture of how this parsing library works


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Structs

(struct parse-derivation
  ;; TODO - document from notes
  (result parser start-index end-index derivation-list)
  #:transparent)

(struct parser
  ;; TODO - document from notes
  (name prefix procedure)
  #:transparent)

(struct alt-parser
  ;; TODO - use a prefix trie for the parsers
  (name parsers extra-arg-lists)
  #:transparent)

(struct parse-stream
  (result next-job scheduler)
  #:methods gen:stream
  [(define (stream-empty? s) #f)
   (define (stream-first s)
     (parse-stream-result s))
   (define (stream-rest s)
     (enter-the-parser/job (parse-stream-scheduler s)
                           (parse-stream-next-job s)))])

(struct parse-failure
  ;; TODO -- what should this contain?
  ()
  #:transparent
  #:methods gen:stream
  [(define (stream-empty? s) #t)
   ;; TODO - better error messages
   (define (stream-first s)
     (error 'stream-first "empty stream"))
   (define (stream-rest s)
     (error 'stream-rest "empty stream"))])

#|
Schedulers keep track of parse work that needs to be done and have caches of results.

The demand stack can contain:
* scheduled-continuations, which have an outer job that they represent (or #f for the original outside caller) and a single dependency.
* alt-workers, which are special workers for alt-parsers.  Alt-workers are spawned instead of normal continuations for alt-parsers.

The job-cache is a multi-tier hash of parser->extra-args-list->start-position->result-number->parser-job
The job->result-cache is a map from parser-job structs -> parser-stream OR parse-error
|#
(struct scheduler
  ;; TODO - document
  (port-broker demand-stack job-info->job-cache job->result-cache)
  #:mutable
  #:transparent)
(define (make-scheduler port-broker)
  (scheduler port-broker '() (hash) (hasheq)))

(struct parser-job
  ;; TODO - document from notes
  (
   ;; parser is either a parser struct or an alt-parser struct
   parser
   extra-arguments
   start-position
   result-index
   [continuation/worker #:mutable]
   [dependents #:mutable]
   )
  #:transparent)
(define (make-parser-job p extra-args start result-index)
  (parser-job p extra-args start result-index #f '()))

(struct alt-worker
  (job remaining-jobs failures successful?)
  #:mutable #:transparent)

(struct scheduled-continuation
  (job k dependency [ready? #:mutable])
  #:transparent)

(define (job->scheduled-continuation j k dep)
  (scheduled-continuation j k dep #f))
(define (fresh-scheduled-continuation k dep)
  (scheduled-continuation #f k dep #f))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Caches

#|
Scheduler cache:
A weak hash port-broker->ephemeron with scheduler.
|#
(define the-scheduler-cache (make-weak-hasheq))
(define port-broker-scheduler
  (make-ephemeron-cache-lookup the-scheduler-cache make-scheduler))

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

(define (scheduler-set-result! s job result)
  (set-scheduler-job->result-cache!
   s
   (hash-set (scheduler-job->result-cache s) job result)))

(define (scheduler-get-result s job)
  (hash-ref (scheduler-job->result-cache s) job #f))

(define (get-job! s parser extra-args start-position result-number)
  (define existing (hash-ref+ #f (scheduler-job-info->job-cache s)
                              parser extra-args start-position result-number))
  (or existing
      (let ([fresh-job (make-parser-job parser extra-args
                                        start-position result-number)])
        (set-scheduler-job-info->job-cache!
         s
         (hash-set+ (scheduler-job-info->job-cache s)
                    parser extra-args start-position result-number fresh-job))
        fresh-job)))

(define (schedule-demand! s demand)
  (set-scheduler-demand-stack! s (cons demand (scheduler-demand-stack s))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Scheduling

(define chido-parse-prompt (make-continuation-prompt-tag 'chido-parse-prompt))
(define chido-parse-k-mark (make-continuation-mark-key 'chido-parse-k-mark))

(define (enter-the-parser port-broker parser extra-args start-position)
  (enter-the-parser/n port-broker parser extra-args start-position 0))

(define (enter-the-parser/n port-broker parser extra-args
                            start-position result-number)
  (define s (port-broker-scheduler port-broker))
  (define j (get-job! s parser extra-args start-position result-number))
  (enter-the-parser/job s j))

(define (enter-the-parser/job scheduler job)
  (or (scheduler-get-result scheduler job)
      #|
      TODO - there is no scheduler done-k iff there is no chido-parse continuation prompt and no chido-parse mark.  In this case I want to capture the whole continuation as done-k and set it on the scheduler.  Otherwise I want to capture a composable continuation, set it as the continuation of whatever job was in the mark, and just re-run the scheduler (probably with new hints).
      |#
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
         (define k-job (if parent-job
                           (job->scheduled-continuation parent-job k parser-job)
                           (fresh-scheduled-continuation k parser-job)))
         (when parent-job
           (set-parser-job-continuation/worker! parent-job k-job))
         (set-parser-job-dependents! job (cons k-job (parser-job-dependents job)))
         (schedule-demand! scheduler k-job)
         (run-scheduler scheduler))
       chido-parse-prompt)))


(define (find-work s job-list)
  ;; Returns an actionable job or #f.
  ;; s is a scheduler
  ;; job-list is a list of parser-job structs
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
  #|
  TODO - my implementation of the demand stack is not quite right.
  The top demand is to get the final result.
  Later demands are recursive calls within procedural parsers.
  If a demand wants the result of an alternative, the alternative might first choose a procedure that recursively wants the same alternative.  That continuation should not be placed on the demand stack, but should just be attached to the job as the way to finish it.  The scheduler should look again for work with the SAME demand since that original job is blocked.  When the demand is finally satisfied (by a different alternative or with [cycle] failure), that continuation will be readied, and if that job is subsequently chosen by something on the demand stack then the continuation will be called.
  Only the original demand is the real demand -- a 3-down recursive call could block on a 1-down recursive call, so the right decision is to schedule an alternative to the 1-down call, but the stack model wil say there is a cycle prematurely.  All cycles should just block until the original demand has no actionable work.  When there is no actionable work at the top level then the smallest innermost cycle should be detected and failed (eg. a biased OR at the bottom could have a cycle as its first choice but its second choice might succeed).
  So there should just be one demand and there is a HINT stack.
  If the demand is ready just return it, otherwise check the bottom (closest to demand) hint for readiness, if ready pop it and all its children off the stack and return.  Otherwise check the next hint.
  The hints are just an optimization.  So what's a better version?  If something fails or becomes blocked the best thing is to pop it off the stack and try a different alternative at the next level of the stack.  If something succeeds maybe it is best to check things higher on the stack to see if they've also become ready.

  TODO - every time I start a job I need to install a continuation prompt and mark.
  TODO - this includes returning to scheduled continuations except for the done-k.
  |#
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
               (run-actionable-job s actionable-job))))]
    [(alt-worker job remaining-jobs failures successful?)
     (cond [(and (null? remaining-jobs) successful?)
            (cache-result-and-ready-dependents! s job empty-stream)
            (pop-demand!)
            (run-scheduler s)]
           [(null? remaining-jobs)
            (define result (alt-worker->failure demanded))
            (cache-result-and-ready-dependents! s job result)
            (pop-demand!)
            (run-scheduler s)]
           [else (let ([actionable-job (find-work s remaining-jobs)])
                   (if (not actionable-job)
                       (let ([cycle-fail (make-cycle-failure job)])
                         (set-alt-worker-failures! demanded
                                                   (cons cycle-fail failures))
                         (pop-demand!)
                         (cache-result-and-ready-dependents!
                          s job (alt-worker->failure demanded))
                         (run-scheduler s))
                       (run-actionable-job s actionable-job)))])]))

(define (run-actionable-job scheduler job)
  ;; TODO - in each branch of this I need to install a prompt and mark.
  (match job
    [(parser-job parser extra-arguments start-position
                 result-index k/worker dependents)
     ;; TODO - validate the parser-job, but without losing the names in the match.
     ;; * k/worker must be #f
     ;; * ... etc
     (match parser
       [(parser name prefix procedure)
        (define port (port-broker->port (scheduler-port-broker scheduler)
                                        start-position))
        ;; TODO - check prefix
        ;; TODO - this interface should maybe be different...
        (define result
          (with-continuation-mark chido-parse-k-mark job
            (with-handlers ([(λ (e) #t) (λ (e) (exn->failure e))])
              (apply procedure port extra-args))))
        (cache-result-and-ready-dependents! scheduler job result)
        (run-scheduler scheduler)]
       [(alt-parser name parsers extra-arg-lists)
        (define (mk-dep parser extra-args)
          (make-parser-job parser extra-args start-position result-index))
        (define worker
          (alt-worker job (map mk-dep parsers extra-arg-lists) '() #f))
        (set-parser-job-continuation/worker! job worker)
        (schedule-demand! scheduler worker)
        (run-scheduler scheduler)])]))

(define (cache-result-and-ready-dependents! scheduler job result)
  ;; TODO - validate result and transform it into an error if it is bad?
  ;; TODO - all results should end up being a parse-stream or parse-failure object.
  ;; TODO - schedule the successor job to this job if the result is a success.
  ;; TODO - if the result is an opaque stream, wrap it with a custom stream such that forcing the stream will call the successor job to force the underlying stream
  ;; TODO - if the result is an empty stream, turn it into a failure result.

  ;; The result ought to be a derivation, a parse-stream with a derivation inside,
  ;; a parse-failure, or some other kind of stream that has a derivation inside.
  ;; TODO - if the result is not a derivation what should happen?
  (if (parse-stream? result)
      TODO
      TODO)
  TODO

  (define worker (let ([w (parser-job-continuation/worker job)])
                   (and (alt-worker? w) w)))
  (set-parser-job-continuation/worker! job #f)
  (scheduler-set-result! scheduler job validated-result)

  (for ([dep (parser-job-dependents job)])
    (match dep
      [(scheduled-continuation job k dependency ready?)
       TODO]
      [(alt-worker alt-job remaining-jobs failures successful?)
       (when (not (parse-failure? result))
         (set-alt-worker-successful? dep #t))
       (set-alt-worker-remaining-jobs! dep (remove job remaining-jobs eq?))
       ;; TODO - if the result is not a failure, I need to ADD a new version of the job to the deps...
       ;; TODO - if a dependent is an alt-worker, recursively cache-and-ready its job, create a new job with the same alt-worker for the next result
       TODO]))
  TODO)

(define (alt-worker->failure aw)
  TODO)

(define (exn->failure e)
  TODO)

(define (make-cycle-failure job)
  TODO)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Parsing outer API

#|
TODO
parse-*/prefix is not a public API, but all the other parse API functions are built on top of it.
|#
;(define (parse-*/prefix TODO) TODO)
;(define (parse-*/whole TODO) TODO)
;(define (parse-1/prefix TODO) TODO)
;(define (parse-1/whole TODO) TODO)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Derived parser contsructors
#|
TODO - derived parser constructors
-- they should all have an optional #:name argument
- sequence parser
- biased alternative parser -- PEG style -- try in order until just one succeeds
- repetition parser
- literal parser -- takes a string
|#
