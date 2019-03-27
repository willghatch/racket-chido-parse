#lang racket/base

(require
 "port-broker.rkt"
 "util.rkt"
 racket/stream
 racket/match
 racket/struct
 racket/exn
 )

;; TODO - explanation from notes about the big picture of how this parsing library works


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Structs

(struct parse-derivation
  ;; TODO - document from notes
  (result parser start-position end-position derivation-list)
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
  (name start-position fail-position message sub-failures)
  #:transparent
  #:methods gen:stream
  [(define (stream-empty? s) #t)
   ;; TODO - better error messages
   (define (stream-first s)
     (error 'stream-first "empty stream"))
   (define (stream-rest s)
     (error 'stream-rest "empty stream"))])

(define (make-cycle-failure job)
  (match job
    [(parser-job parse extra-arguments start-position result-index
                 k/worker dependents stream-stack)
     (match parse
       [(alt-parser name parsers extra-arg-lists)
        (parse-failure name start-position start-position "Cycle failure" '())]
       [(parser name prefix procedure)
        (parse-failure name start-position start-position "cycle-failure" '())])]))

(define (exn->failure e job)
  (let ([message (format "Exception while parsing: ~a\n" (exn->string e))])
    (match job
      [(parser-job parse extra-arguments start-position
                   result-index continuation/worker dependents stream-stack)
       (match parse
         ;; TODO - it should only be possible to get a procedural parser here, not an alt-parser.
         [(parser name prefix procedure)
          (parse-failure name start-position start-position message '())])])))

(define (alt-worker->failure aw)
  (match aw
    [(alt-worker job remaining-jobs ready-jobs failures successful?)
     (match job
       [(parser-job parser extra-arguments start-position result-index
                    k/worker dependents stream-stack)
        (match parser
          [(alt-parser name parsers extra-arg-lists)
           ;; TODO - what is the best fail position?
           ;;        Probably I should analyze the sub-failures...
           (define fail-position start-position)
           (parse-failure name start-position fail-position
                          "TODO - better failure message" failures)])])]))


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
  (port-broker done-k hint-stack job-info->job-cache job->result-cache)
  #:mutable
  #:transparent)
(define (make-scheduler port-broker)
  (scheduler port-broker #f '() (hash) (hasheq)))

(define (pop-hint! scheduler)
  (set-scheduler-hint-stack! scheduler (cdr (scheduler-hint-stack scheduler))))
(define (push-hint! scheduler hint)
  (set-scheduler-hint-stack! scheduler (cons hint (scheduler-hint-stack scheduler))))

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
   [stream-stack #:mutable]
   )
  #:transparent)
(define (make-parser-job p extra-args start result-index)
  (parser-job p extra-args start result-index #f '() '()))

(struct cycle-breaker-job (failure-job) #:transparent)

(struct alt-worker
  (job remaining-jobs ready-jobs failures successful?)
  #:mutable #:transparent)

(struct scheduled-continuation
  ;; The dependency is only mutated to break cycles.
  (job k [dependency #:mutable] [ready? #:mutable])
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
                            (hash-ref h k1 (λ () (hash)))
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
  (match job
    [(cycle-breaker-job failure-job)
     (make-cycle-failure failure-job)]
    [else (hash-ref (scheduler-job->result-cache s) job #f)]))

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

(define (get-next-job! s job)
  (match job
    [(parser-job parser extra-args start-position result-index k/w deps s-stack)
     (get-job! s parser extra-args start-position (add1 result-index))]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Scheduling

(define chido-parse-prompt (make-continuation-prompt-tag 'chido-parse-prompt))
(define chido-parse-k-mark (make-continuation-mark-key 'chido-parse-k-mark))
(define recursive-enter-flag (gensym))

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
      (if (scheduler-done-k scheduler)
          ;; This is a recursive call.
          ;; So we de-schedule the current work by capturing its continuation,
          ;; we schedule its dependency, and then we abort back to the
          ;; scheduler loop.
          (call-with-composable-continuation
           (λ (k)
             (define k-marks (continuation-marks k))
             (define outer-job-marks (continuation-mark-set->list k-marks
                                                                  chido-parse-k-mark
                                                                  chido-parse-prompt))
             (define parent-job (match outer-job-marks
                                  ['() (error
                                        'chido-parse
                                        "internal error -- no continuation mark in recursive call")]
                                  [(list j) j]
                                  [else (error
                                         'chido-parse
                                         "internal error -- bad continuation marks")]))
             (define k-job (job->scheduled-continuation parent-job k parser-job))
             (set-parser-job-continuation/worker! parent-job k-job)
             (set-parser-job-dependents! job (cons k-job (parser-job-dependents job)))
             (push-hint! scheduler k-job)
             ;; Launch the scheduler by being "done" with a flag value.
             ((scheduled-continuation-k (scheduler-done-k scheduler))
              recursive-enter-flag))
           chido-parse-prompt)
          (let loop ()
            ;; This is the original entry into the parser machinery.
            ;; In this branch we want to capture the full continuation.
            ;; But we do it in this loop to keep from growing a little bit
            ;; of extra continuation on each recursive call.
            (define result
              (call-with-current-continuation
               (λ (full-k)
                 (define k-job (fresh-scheduled-continuation full-k parser-job))
                 (set-parser-job-dependents!
                  job
                  (cons k-job (parser-job-dependents job)))
                 ;; As a fresh entry into the parser, we start a fresh hint stack.
                 (set-scheduler-hint-stack! scheduler (list k-job))
                 (run-scheduler scheduler))))
            (if (eq? recursive-enter-flag result)
                (loop)
                result)))))


(define (find-work s job-list)
  ;; Returns an actionable job or #f.
  ;; s is a scheduler
  ;; job-list is a list of parser-job structs
  ;; TODO - this is probably the best place to detect cycles.  I should maybe return some sort of flag object containing the job that contains the smallest dependency cycle so I know which job to return a cycle error for.
  (let loop ([jobs job-list]
             [blocked '()])
    (if (null? jobs)
        #f
        (let ([j (car jobs)])
          (cond [(member j blocked) (loop (cdr jobs) blocked)]
                ;; if it has a continuation/worker, add dependencies to jobs
                [else (match (parser-job-continuation/worker j)
                        [(scheduled-continuation job k dependency ready?)
                         (cond [ready? j]
                               [else (loop (cons dependency (cdr jobs))
                                           (cons j blocked))])]
                        [(alt-worker job remaining-jobs ready-jobs
                                     failures successful?)
                         (cond [(not (null? ready-jobs)) (car ready-jobs)]
                               [(null? remaining-jobs) j]
                               [else (loop (append remaining-jobs jobs)
                                           (cons j blocked))])]
                        [#f j])])))))

(define (run-scheduler s)
  (define using-hint? #t)
  (when (null? (scheduler-hint-stack s))
    (set! using-hint? #f)
    (set-scheduler-hint-stack! s (list (scheduler-done-k s))))
  (match (car (scheduler-hint-stack s))
    [(scheduled-continuation #f done-k dep #t)
     ;; done-k is ready.
     (set-scheduler-done-k! s #f)
     (done-k (scheduler-get-result s dep))]
    [(scheduled-continuation job k dependency #t)
     ;; also ready
     (pop-hint! s)
     (run-actionable-job s job)]
    [(scheduled-continuation job k dependency ready?)
     ;; Not ready
     (let ([actionable-job (find-work s (list dependency))])
       (cond [actionable-job (run-actionable-job s actionable-job)]
             [using-hint? (set-scheduler-hint-stack! s '())
                          (run-scheduler s)]
             [else (fail-smallest-cycle! s)
                   (run-scheduler s)]))]
    [(alt-worker job remaining-jobs (list ready-job rjs ...) failures successful?)
     (pop-hint! s)
     (run-actionable-job s job)]
    [(alt-worker job (list) (list) failures successful?)
     (pop-hint! s)
     (run-actionable-job s job)]
    [(alt-worker job remaining-jobs ready-jobs failures successful?)
     (let ([actionable-job (find-work s remaining-jobs)])
       (cond [actionable-job
              (run-actionable-job s actionable-job)]
             ;; Try finding an actionable job without following hints
             [using-hint? (set-scheduler-hint-stack! s '())
                          (run-scheduler s)]
             [else (fail-smallest-cycle! s)
                   (run-scheduler s)]))]))


(define (fail-smallest-cycle! scheduler)
  #|
  TODO - better name.
  This isn't failing the smallest cycle necessarily, but it is failing the job in a cycle that first depends back on something earlier in the chain to the root goal.
  |#
  (define (rec goal jobs)
    (match goal
      [(alt-worker job remaining-jobs ready-jobs failures successful?)
       ;; If I'm in a situation to have to break a cycle, all remaining-jobs
       ;; are cyclic.  So let's just break the first one for now.
       (define next-job (car remaining-jobs))
       (rec (parser-job-continuation/worker next-job) (cons job jobs))]
      [(scheduled-continuation job k dependency ready?)
       (if (member dependency jobs)
           (begin (set-scheduled-continuation-dependency!
                   goal (cycle-breaker-job dependency))
                  (set-scheduled-continuation-ready?! goal #t))
           (rec (parser-job-continuation/worker dependency)
                (cons job jobs)))]))
  (rec (scheduler-done-k scheduler) '()))

(define (run-actionable-job scheduler job)
  (match job
    [(parser-job parsador extra-args start-position
                 result-index k/worker dependents s-stack)
     (match k/worker
       [(scheduled-continuation job k dependency (and ready? #t))
        (do-run! scheduler
                 (λ () (k (scheduler-get-result scheduler dependency)))
                 job
                 s-stack)]
       [(alt-worker job remaining-jobs (list ready-job rjs ...) failures successful?)
        ;; TODO - remove ready-job from ready-jobs and remaining-jobs, if it's a success cache the result, set success flag, and add the next iteration of the job to the remaining jobs (this should check if it's also ready and add it to the ready list as appropriate), if failure add to failures.
        (set-alt-worker-remaining-jobs! k/worker (remove ready-job remaining-jobs))
        (set-alt-worker-ready-jobs! k/worker rjs)
        ;; TODO - be sure I'm getting the raw result, not the stream
        (define result (scheduler-get-result scheduler ready-job))
        (if (parse-failure? result)
            (set-alt-worker-failures! k/worker (cons result failures))
            (let ([this-next-job (get-next-job! scheduler job)]
                  [dep-next-job (get-next-job! scheduler ready-job)])
              (set-alt-worker-successful?! k/worker #t)
              (cache-result-and-ready-dependents! scheduler job result)
              (set-alt-worker-job! k/worker this-next-job)
              (set-parser-job-continuation/worker! this-next-job k/worker)
              (set-alt-worker-remaining-jobs!
               k/worker
               (cons dep-next-job (alt-worker-remaining-jobs k/worker)))
              (when (scheduler-get-result scheduler dep-next-job)
                (set-alt-worker-ready-jobs!
                 k/worker
                 (cons dep-next-job (alt-worker-ready-jobs k/worker))))))
        (run-scheduler scheduler)]
       [(alt-worker job (list) (list) failures successful?)
        ;; Finished alt-worker.
        ;; TODO - Right now a failure object is also an empty stream,
        ;;        so it can serve both cases.  Should this change?
        (define result (alt-worker->failure k/worker))
        (cache-result-and-ready-dependents! scheduler job result)
        (run-scheduler scheduler)]
       [else
        (match s-stack
          [(list stream1 stack-rest ...)
           (set-parser-job-stream-stack! job stack-rest)
           (do-run! scheduler (λ () (stream-rest stream1)) job stack-rest)]
          [else
           (match parsador
             [(parser name prefix procedure)
              (define port (port-broker->port (scheduler-port-broker scheduler)
                                              start-position))
              ;; TODO - check prefix.  All parsers that fail the prefix check can be logged as failures immediately, then the remaining parsers can be prioritized by length of prefix.
              ;; TODO - this interface should maybe be different...
              (do-run! scheduler
                       (λ () (apply procedure port extra-args))
                       job
                       '())]
             [(alt-parser name parsers extra-arg-lists)
              (define (mk-dep parsador extra-args)
                (make-parser-job parsador extra-args start-position result-index))
              (define worker
                (alt-worker job (map mk-dep parsers extra-arg-lists) '() '() #f))
              (set-parser-job-continuation/worker! job worker)
              (push-hint! scheduler worker)
              (run-scheduler scheduler)])])])]))

(define (do-run! scheduler thunk job stream-stack)
  (define result
    (call-with-continuation-prompt
     (λ ()
       (with-handlers ([(λ (e) #t) (λ (e) (exn->failure e job))])
         (with-continuation-mark chido-parse-k-mark job
           (thunk))))
     chido-parse-prompt))
  (cache-result-and-ready-dependents! scheduler job result stream-stack)
  (run-scheduler scheduler))

(define (ready-dependents! job)
  (for ([dep (parser-job-dependents job)])
    (match dep
      [(alt-worker job remaining-jobs ready-jobs failures successful?)
       (set-alt-worker-ready-jobs! dep (cons job ready-jobs))]
      [(scheduled-continuation job k dependency ready?)
       (set-scheduled-continuation-ready?! dep #t)]))
  (set-parser-job-dependents! job '()))

(define (cache-result-and-ready-dependents! scheduler job result [stream-stack '()])
  (if (alt-parser? (parser-job-parser job))
      (cache-result-and-ready-dependents!/alt-job scheduler job result)
      (cache-result-and-ready-dependents!/procedure-job
       scheduler job result stream-stack)))

(define (cache-result-and-ready-dependents!/alt-job scheduler job result)
  ;; This version only gets pre-sanitized results, and is straightforward.
  (scheduler-set-result! scheduler job result)
  (ready-dependents! job))

(define (cache-result-and-ready-dependents!/procedure-job
         scheduler job result [stream-stack '()])
  (match result
    [(parse-failure name start-position fail-position message sub-failures)
     (match stream-stack
       [(list stream1 streams ...)
        ;; Pop the stream stack and start working on the next one.
        ;; In this case we DON'T ready dependents!
        ;; So the job will essentially re-run with a smaller stream stack.
        ;; (The stream-stack that comes in is one smaller than the original
        ;; stream-stack from the job.)
        (set-parser-job-stream-stack! job stream-stack)]
       [else
        (scheduler-set-result! scheduler job result)
        (ready-dependents! job)])]
    [(parse-stream inner-result next-job sched)
     ;; TODO
     ;; If this parse stream comes from the same job family
     ;; (same parser, position, etc, just different index number),
     ;; then this will cause a stupid infinite cycle of results,
     ;; and we want to kill that.
     ;; Otherwise it should be treated as an ordinary stream.
     (cache-result-and-ready-dependents!/procedure-job
      scheduler job inner-result (cons result stream-stack))]
    [(? (λ (x) (and (stream? x) (stream-empty? x))))
     ;; Turn it into a parse failure and recur.
     (match job
       [(parser-job parse extra-arguments start-position
                    result-index continuation/worker
                    dependents job-stream-stack)
        (match parse
          [(parser name prefix procedure)
           (cache-result-and-ready-dependents!/procedure-job
            scheduler
            job
            (parse-failure name start-position start-position
                           "parse returned empty stream" '())
            stream-stack)])])]
    [(? stream?)
     ;; Recur with stream-first, adding the stream to the stream-stack.
     (cache-result-and-ready-dependents!/procedure-job
      scheduler job (stream-first result) (cons result stream-stack))]
    [(parse-derivation result parser start-position end-position derivation-list)
     ;; This is the main branch.
     ;; Use the stream-stack to determine the continuation to get the next result.
     ;; Package it up in a parse-stream object.
     (define next-job (get-next-job! scheduler job))
     (set-parser-job-stream-stack! next-job stream-stack)
     (define wrapped-result
       (parse-stream result next-job scheduler))
     (scheduler-set-result! scheduler job wrapped-result)
     (ready-dependents! job)]
    [else (error 'chido-parse "Parsers must (for now) return parse-derivation objects")]
    #;[else
     ;; Turn the result into a parse-derivation and recur.
     (define parser (parser-job-parser job))
     (define start-position (parser-job-start-position job))
     (define end-position TODO-aoeu)
     (define new-result
       (parse-derivation result parser start-position end-position '()))
     (cache-result-and-ready-dependents!/procedure-job
      scheduler job new-result stream-stack)]))



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
