#lang racket/base

(require
 "port-broker.rkt"
 "util.rkt"
 "stream-flatten.rkt"
 racket/stream
 racket/match
 racket/struct
 racket/exn
 (for-syntax
  racket/base
  syntax/parse
  ))

(module+ test
  (require
   rackunit
   "forparse.rkt"
   ))

;; TODO - explanation from notes about the big picture of how this parsing library works

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Structs

(struct parse-derivation
  ;; TODO - document from notes
  (result parser start-position end-position derivation-list)
  #:transparent)

(define (make-parse-derivation result
                               #:end [end #f]
                               #:derivations [derivation-list '()])
  (define job (current-chido-parse-job))
  (match job
    [(parser-job parser extra-arguments start-position result-index
                 continuation/worker dependents result-stream)
     (define end-use (or end
                         (and (not (null? derivation-list))
                              (apply max
                                     (map parse-derivation-end-position
                                          derivation-list)))
                         (error 'chido-parse "TODO - for now you have to supply an end location explicitly when making a derivation")))
     (parse-derivation result parser start-position end-use derivation-list)]))

(struct parser (name) #:transparent)

(struct proc-parser parser
  ;; TODO - document from notes
  (prefix procedure)
  #:transparent)

(struct alt-parser parser
  ;; TODO - use a prefix trie for the parsers
  (parsers extra-arg-lists)
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

(define (make-parse-failure message #:position [pos #f] #:failures [failures '()])
  (define job (current-chido-parse-job))
  (match job
    [(parser-job parser extra-arguments start-position result-index
                 continuation/worker dependents result-stream)
     (parse-failure (parser-name parser) start-position (or pos start-position)
                    message failures)]))

(define (make-cycle-failure job)
  (match job
    [(parser-job parser extra-arguments start-position result-index
                 k/worker dependents result-stream)
     (parse-failure (parser-name parser)
                    start-position
                    start-position
                    "Cycle failure"
                    '())]))

(define (exn->failure e job)
  (let ([message (format "Exception while parsing: ~a\n" (exn->string e))])
    (match job
      [(parser-job parser extra-arguments start-position
                   result-index continuation/worker dependents result-stream)
       (parse-failure (parser-name parser)
                      start-position start-position message '())])))

(define (alt-worker->failure aw)
  (match aw
    [(alt-worker job remaining-jobs ready-jobs failures successful?)
     (match job
       [(parser-job parser extra-arguments start-position result-index
                    k/worker dependents result-stream)
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
   ;; dependents are scheduled-continuations or alt-workers
   [dependents #:mutable]
   ;; This one is not used for alt-parsers, but is used to keep track of the
   ;; stream from a procedure.
   [result-stream #:mutable]
   )
  #:transparent)

(define (push-parser-job-dependent! job new-dependent)
  (set-parser-job-dependents!
   job
   (cons new-dependent (parser-job-dependents job))))

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


(define (job->parser-name job)
  (define p (and job (parser-job-parser job)))
  (if (parser? p)
      (parser-name p)
      p))
(define (job->display job)
  (cond [(not job) #f]
        [(cycle-breaker-job? job) "cycle-breaker"]
        [else (format "~a@~a_~a"
                      (job->parser-name job)
                      (parser-job-start-position job)
                      (parser-job-result-index job))]))



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
(define hash-ref+-flag (gensym))
(define (hash-ref+ fail-thunk h k1 . args)
  (if (null? args)
      (hash-ref h k1 fail-thunk)
      (let ([hr1 (hash-ref h k1 hash-ref+-flag)])
        (if (eq? hr1 hash-ref+-flag)
            (if (procedure? fail-thunk)
                (fail-thunk)
                fail-thunk)
            (apply hash-ref+ fail-thunk hr1 args)))))

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
  (define (make-parser-job p extra-args start result-index)
    (parser-job p extra-args start result-index #f '() #f))
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
    [(parser-job parser extra-args start-position result-index k/w deps result-stream)
     (get-job! s parser extra-args start-position (add1 result-index))]))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Scheduling

(define chido-parse-prompt (make-continuation-prompt-tag 'chido-parse-prompt))
(define current-chido-parse-job (make-parameter #f))
(define recursive-enter-flag (gensym 'recursive-enter-flag))

(define (enter-the-parser port-broker parser extra-args start-position)
  (define s (port-broker-scheduler port-broker))
  (define j (get-job! s parser extra-args start-position 0))
  (enter-the-parser/job s j))

(define (enter-the-parser/job scheduler job)
  (define ready-result (scheduler-get-result scheduler job))
  (or ready-result
      #|
      TODO - there is no scheduler done-k iff there is no chido-parse continuation prompt and no chido-parse mark.  In this case I want to capture the whole continuation as done-k and set it on the scheduler.  Otherwise I want to capture a composable continuation, set it as the continuation of whatever job was in the mark, and just re-run the scheduler (probably with new hints).
      |#
      (if (scheduler-done-k scheduler)
          ;; This is a recursive call.
          ;; So we de-schedule the current work by capturing its continuation,
          ;; we schedule its dependency, and then we abort back to the
          ;; scheduler loop.
          (let ([parent-job (current-chido-parse-job)])
            (call-with-composable-continuation
             (λ (k)
               (define sched-k (job->scheduled-continuation parent-job k job))
               (set-parser-job-continuation/worker! parent-job sched-k)
               (push-parser-job-dependent! job sched-k)
               ;(push-hint! scheduler sched-k)
               ;; Launch the scheduler by being "done" with a flag value.
               ((scheduled-continuation-k (scheduler-done-k scheduler))
                recursive-enter-flag))
             chido-parse-prompt))
          ;; This is the original entry into the parser machinery.
          ;; In this branch we want to capture the full continuation.
          ;; We also use this continuation as a loop point for the
          ;; scheduler until a result is ready.
          (let ()
            (define result
              (call-with-current-continuation
               (λ (full-k)
                 (define k-job (scheduled-continuation #f full-k job #f))
                 (push-parser-job-dependent! job k-job)
                 ;; As a fresh entry into the parser, we start a fresh hint stack.
                 (set-scheduler-hint-stack! scheduler (list k-job))
                 (set-scheduler-done-k! scheduler k-job)
                 (run-scheduler scheduler))))
            (if (eq? recursive-enter-flag result)
                (run-scheduler scheduler)
                (begin
                  (set-scheduler-done-k! scheduler #f)
                  result))))))


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
                         (cond [(not (null? ready-jobs)) j]
                               [(null? remaining-jobs) j]
                               [else (loop (append remaining-jobs jobs)
                                           (cons j blocked))])]
                        [#f j])])))))

(define (run-scheduler s)
  (define using-hint? #t)
  (when (null? (scheduler-hint-stack s))
    (set! using-hint? #f)
    (set-scheduler-hint-stack! s (list (scheduler-done-k s))))
  ;(define hint (car (scheduler-hint-stack s)))
  (define hint (scheduler-done-k s))
  (match hint
    [(scheduled-continuation #f done-k dep #t)
     ;; done-k is ready.
     (define result (scheduler-get-result s dep))
     (done-k result)]
    [(scheduled-continuation job k dependency #t)
     ;; also ready
     ;(pop-hint! s)
     (run-actionable-job s job)]
    [(scheduled-continuation job k dependency ready?)
     ;; Not ready
     (let ([actionable-job (find-work s (list dependency))])
       (cond
         [(and actionable-job (scheduler-get-result s actionable-job))
          (eprintf "\n")
          (eprintf "WARNING! A continuation was not marked ready when its dependency finished\n")
          (eprintf "Continuation for job: ~a\n" (and job (job->display job)))
          (eprintf "Dependency that didn't mark it ready: ~a\n"
                   (job->display actionable-job))
          (eprintf "Result for the dependency: ~s\n"
                   (scheduler-get-result s actionable-job))
          (eprintf "\n\n")
          (error 'chido-parse "Internal error, dependency tracking error")]
         [actionable-job (run-actionable-job s actionable-job)]
         [using-hint? (set-scheduler-hint-stack! s '())
                      (run-scheduler s)]
         [else (fail-smallest-cycle! s)
               (run-scheduler s)]))]
    [(alt-worker job remaining-jobs (list ready-job rjs ...) failures successful?)
     ;(pop-hint! s)
     (run-actionable-job s job)]
    [(alt-worker job (list) (list) failures successful?)
     ;(pop-hint! s)
     (run-actionable-job s job)]
    [(alt-worker job remaining-jobs ready-jobs failures successful?)
     (let ([actionable-job (find-work s remaining-jobs)])
       (cond
         [(and actionable-job (scheduler-get-result s actionable-job))
          (eprintf "\n")
          (eprintf "WARNING! An alt-worker was not marked ready when its dependency finished\n")
          (eprintf "Worker for job: ~a\n" (job->display job))
          (eprintf "Dependency that didn't mark it ready: ~a\n"
                   (job->display actionable-job))
          (eprintf "result for the dependency: ~s\n" (scheduler-get-result s actionable-job))
          (eprintf "\n\n")
          (error 'chido-parse "Internal error -- dependency tracking issue")]
         [actionable-job
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
           (begin
             (set-scheduled-continuation-dependency!
              goal
              (cycle-breaker-job dependency))
             (set-scheduled-continuation-ready?! goal #t))
           (rec (parser-job-continuation/worker dependency)
                (cons job jobs)))]))
  (rec (scheduler-done-k scheduler) '()))

(define (run-actionable-job scheduler job)
  (when (scheduler-get-result scheduler job)
    (error 'chido-parse
           "internal error - run-actionable-job got a job that was already done: ~a"
           (job->display job)))
  (match job
    [(parser-job parsador extra-args start-position
                 result-index k/worker dependents result-stream)
     (match k/worker
       [(scheduled-continuation job k dependency (and ready? #t))
        (do-run! scheduler
                 (λ () (k (scheduler-get-result scheduler dependency)))
                 job)]
       [(alt-worker job remaining-jobs (list ready-job rjs ...) failures successful?)
        ;; TODO - remove ready-job from ready-jobs and remaining-jobs, if it's a success cache the result, set success flag, and add the next iteration of the job to the remaining jobs (this should check if it's also ready and add it to the ready list as appropriate), if failure add to failures.
        (set-alt-worker-remaining-jobs! k/worker (remove ready-job remaining-jobs))
        (set-alt-worker-ready-jobs! k/worker rjs)
        (define result (scheduler-get-result scheduler ready-job))
        (cond
          [(parse-failure? result)
           (set-alt-worker-failures! k/worker (cons result failures))]
          [(parse-stream? result)
           (let ([result-contents (stream-first result)]
                 [this-next-job (get-next-job! scheduler job)]
                 [dep-next-job (get-next-job! scheduler ready-job)])
             (define result-stream
               (parse-stream result-contents this-next-job scheduler))
             (cache-result-and-ready-dependents! scheduler job result-stream)
             (set-alt-worker-successful?! k/worker #t)
             (set-alt-worker-job! k/worker this-next-job)
             (set-parser-job-continuation/worker! this-next-job k/worker)
             (set-parser-job-continuation/worker! job #f)
             (push-parser-job-dependent! dep-next-job k/worker)
             (set-alt-worker-remaining-jobs!
              k/worker
              (cons dep-next-job (alt-worker-remaining-jobs k/worker)))
             (when (scheduler-get-result scheduler dep-next-job)
               (set-alt-worker-ready-jobs!
                k/worker
                (cons dep-next-job (alt-worker-ready-jobs k/worker)))))]
          [else
           (error 'chido-parse
                  "Internal error - alt-worker got a non-stream result: ~s"
                  result)])
        (run-scheduler scheduler)]
       [(alt-worker job (list) (list) failures successful?)
        ;; Finished alt-worker.
        ;; TODO - Right now a failure object is also an empty stream,
        ;;        so it can serve both cases.  Should this change?
        (define result (alt-worker->failure k/worker))
        (cache-result-and-ready-dependents! scheduler job result)
        (run-scheduler scheduler)]
       [#f
        (cond [(stream? result-stream)
               (do-run! scheduler
                        (λ () (stream-rest result-stream))
                        job)]
              [(equal? 0 result-index)
               (match parsador
                 [(proc-parser name prefix procedure)
                  (define port (port-broker->port/char (scheduler-port-broker
                                                        scheduler)
                                                       start-position))
                  ;; TODO - check prefix.  All parsers that fail the prefix check can be logged as failures immediately, then the remaining parsers can be prioritized by length of prefix.
                  ;; TODO - this interface should maybe be different...
                  (do-run! scheduler
                           (λ () (let ([result (apply procedure port extra-args)])
                                   (if (and (stream? result)
                                            (not (flattened-stream? result)))
                                       (stream-flatten result)
                                       result)))
                           job)]
                 [(alt-parser name parsers extra-arg-lists)
                  (define (mk-dep parsador extra-args)
                    (get-job! scheduler parsador extra-args start-position 0))
                  (define worker
                    (alt-worker job (map mk-dep parsers extra-arg-lists) '() '() #f))
                  (for ([dep (alt-worker-remaining-jobs worker)])
                    (push-parser-job-dependent! dep worker))
                  (set-parser-job-continuation/worker! job worker)
                  ;(push-hint! scheduler worker)
                  (run-scheduler scheduler)])]
              [else
               ;; In this case there has been a result stream but it is dried up.
               (let ([end-failure (parse-failure (job->parser-name job)
                                                 (parser-job-start-position job)
                                                 (parser-job-start-position job)
                                                 "No more results"
                                                 '())])
                 (cache-result-and-ready-dependents! scheduler
                                                     job
                                                     end-failure)
                 (set-parser-job-continuation/worker! job #f)
                 (run-scheduler scheduler))])])]))

(define (do-run! scheduler thunk job)
  (when (not job)
    (error 'chido-parse "Internal error - trying to recur with no job"))
  (define result
    (call-with-continuation-prompt
     (λ ()
       (with-handlers ([(λ (e) #t) (λ (e) (exn->failure e job))])
         (parameterize ([current-chido-parse-job job])
           (thunk))))
     chido-parse-prompt))
  (cache-result-and-ready-dependents! scheduler job result)
  (run-scheduler scheduler))

(define (ready-dependents! job)
  (for ([dep (parser-job-dependents job)])
    (match dep
      [(alt-worker alt-job remaining-jobs ready-jobs failures successful?)
       (set-alt-worker-ready-jobs! dep (cons job ready-jobs))]
      [(scheduled-continuation parent-job k dependency ready?)
       (set-scheduled-continuation-ready?! dep #t)]))
  (set-parser-job-dependents! job '()))

(define (cache-result-and-ready-dependents! scheduler job result)
  (if (alt-parser? (parser-job-parser job))
      (cache-result-and-ready-dependents!/alt-job scheduler job result)
      (cache-result-and-ready-dependents!/procedure-job
       scheduler job result)))

(define (cache-result-and-ready-dependents!/alt-job scheduler job result)
  ;; This version only gets pre-sanitized results, and is straightforward.
  (scheduler-set-result! scheduler job result)
  (ready-dependents! job))

(define (cache-result-and-ready-dependents!/procedure-job
         scheduler job result)
  (match result
    [(parse-failure name start-position fail-position message sub-failures)
     (scheduler-set-result! scheduler job result)
     (ready-dependents! job)]
    [(? (λ (x) (and (stream? x) (stream-empty? x))))
     ;; Turn it into a parse failure and recur.
     (match job
       [(parser-job parse extra-arguments start-position
                    result-index continuation/worker
                    dependents result-stream)
        (match parse
          [(proc-parser name prefix procedure)
           (cache-result-and-ready-dependents!/procedure-job
            scheduler
            job
            (parse-failure name start-position start-position
                           "parse returned empty stream" '()))])])]
    [(? stream?)
     ;; Recur with stream-first, setting the stream as the result-stream
     (define next-job (get-next-job! scheduler job))
     (set-parser-job-result-stream! next-job result)
     (cache-result-and-ready-dependents!/procedure-job
      scheduler job (raw-result->parse-derivation (stream-first result)))]
    [(parse-derivation semantic-result parser start-position
                       end-position derivation-list)
     (define next-job (get-next-job! scheduler job))
     (define wrapped-result
       (parse-stream result next-job scheduler))
     (scheduler-set-result! scheduler job wrapped-result)
     (ready-dependents! job)]
    [else (cache-result-and-ready-dependents! scheduler
                                              job
                                              (raw-result->parse-derivation result))]))

(define (raw-result->parse-derivation result)
  (if (parse-derivation? result)
      result
      (error 'TODO "auto-transformation to proper parse result not yet implemented")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Parsing outer API

(define (parse* port parser
                #:args [extra-args '()]
                #:start [start #f])
  (define pb (or (port->port-broker port)
                 (port-broker port)))
  (define start-pos (match start
                      [(? parse-derivation?) (parse-derivation-end-position start)]
                      [#f (let-values ([(line col pos) (port-next-location port)])
                            pos)]))
  (enter-the-parser pb parser extra-args start-pos))

#|
TODO
|#
;(define (parse TODO) TODO)
;(define (parse* TODO) TODO)
;(define (whole-parse TODO) TODO)
;(define (whole-parse* TODO) TODO)


(module+ test
  (define s1 "aaaaa")
  (define p1 (open-input-string s1))
  (port-count-lines! p1)

  (define (a1-parser-proc port)
    (define-values (line col pos) (port-next-location port))
    (define c (read-char port))
    (if (equal? c #\a)
        (make-parse-derivation "a"
                               #:end (add1 pos)
                               #:derivations '())
        (make-parse-failure "Didn't match.")))
  (define a1-parser-obj (proc-parser "a" "" a1-parser-proc))

  (define (Aa-parser-proc port)
    (for/parse
     ([d/A (parse* port (get-A-parser))])
     (for/parse
      ([d/a (parse* port a1-parser-obj
                    #:start d/A)])
      (make-parse-derivation (string-append (parse-derivation-result d/A)
                                            (parse-derivation-result d/a))
                             #:derivations (list d/A d/a)))))

  (define Aa-parser-obj (proc-parser "Aa" "" Aa-parser-proc))


  (define A-parser (alt-parser "A"
                               (list
                                Aa-parser-obj
                                a1-parser-obj
                                )
                               (list '() '())))
  (define (get-A-parser) A-parser)

  (define results1 (parse* p1 A-parser))

  ;(printf "\n\n")
  ;(printf "r1-1: ~s\n" (stream-ref results1 0))
  ;(printf "r1-2: ~s\n" (stream-ref results1 1))
  ;(printf "r1-3: ~s\n" (stream-ref results1 2))
  ;(printf "\n\n")
  ;(printf "r1-2: ~a\n" (stream-ref results1 1))

  (eprintf "going to print all result now.......................................\n")
  (let loop ([stream results1])
    (when (not (stream-empty? stream))
      (printf "result: ~s\n" (parse-derivation-result (stream-first stream)))
      (loop (stream-rest stream))))

  )


;; Temporarily
(module+ main
  (require (submod ".." test))
  )


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
