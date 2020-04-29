#lang racket/base

#|
Simplifications from the full core.rkt:
* Error handling - there is only one error object which contains no useful data
* derivation objects - results are strict instead of potentially lazy, only start/end positions are tracked (no line/column/span/source-name)
* proc-parsers and alt-parsers have no extra metadata like prefix, nullability, promise-no-left-recursion, prefix tries, etc
* no custom parser structs or raw strings (but still supporting parser thunks because they are useful to “tie the knot”)
* the scheduler always captures continuations, not checking whether left recursion is possible
* jobs don't track their dependents to update them automatically - we just do a search for available work each time we enter the scheduler
* TODO - no chido-parse-parameters
* TODO - maybe switch to string input instead of ports (needs a change from parsers being (-> port derivation) to (-> string position derivation))
|#


(require
 "port-broker.rkt"
 "util.rkt"
 "parse-failure.rkt"
 "stream-flatten.rkt"
 "parameters.rkt"
 "trie.rkt"
 "parse-stream.rkt"
 racket/stream
 racket/port
 racket/match
 racket/list
 racket/struct
 racket/string
 racket/exn
 data/gvector
 (rename-in kw-make-struct
            [make/kw s/kw])
 (for-syntax
  racket/base
  syntax/parse
  racket/syntax
  ))

(module+ test
  (require
   rackunit
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Structs

(struct parse-derivation
  (result
   parser
   start-position end-position
   derivation-list)
  #:transparent)


(define current-chido-parse-derivation-implicit-end (make-parameter #f))

(define (make-parse-derivation result
                               #:end [end #f]
                               #:derivations [derivations '()])
  (define job (current-chido-parse-job))
  (define derivation-list (if (list? derivations) derivations (list derivations)))
  ;; TODO - simplify end handling.
  (match job
    [(s/kw parser-job
           #:parser parser
           #:scheduler scheduler
           #:start-position start-position)
     (define pb (scheduler-port-broker scheduler))
     (define source-name (port-broker-source-name pb))
     (define line (port-broker-line pb start-position))
     (define column (port-broker-column pb start-position))
     (define end-use (or end
                         (and (not (null? derivation-list))
                              (apply max
                                     (map parse-derivation-end-position
                                          derivation-list)))
                         (current-chido-parse-derivation-implicit-end)
                         (let ([port (parser-job-port job)])
                           (and port (port->pos port)))
                         (error 'make-parse-derivation
                                "Couldn't infer end location and none provided.")))
     (define delayed? (procedure? result))
     (parse-derivation result
                       parser
                       start-position end-use
                       derivation-list)]
    [else (error 'make-parse-derivation
                 "Not called during the dynamic extent of chido-parse...")]))

(struct parser-struct (name) #:transparent)

(struct proc-parser parser-struct (procedure) #:transparent)
(struct alt-parser parser-struct (parsers) #:transparent)


(define (parser? p)
  (cond [(parser-struct? p)]
        ;; TODO - this is not a great predicate...
        [(procedure? p)]
        [else #f]))

(define (parser-name p)
  (cond [(parser-struct? p) (parser-struct-name p)]
        [(procedure? p) (parser-name (p))]
        [else (error 'parser-name "not a parser: ~s" p)])) 


(define parser-cache (make-weak-hasheq))
(define (parser->usable p)
  (cond [(parser-struct? p) p]
        [(and (procedure? p) (procedure-arity-includes? p 0))
         (define cached (hash-ref parser-cache p #f))
         (or (and cached (parser->usable cached))
             (let ([result (p)])
               (hash-set! parser-cache p result)
               (parser->usable result)))]
        [else (error 'chido-parse "not a parser: ~s" p)]))

(struct parse-stream
  (result next-job scheduler)
  #:methods gen:stream
  [(define (stream-empty? s) #f)
   (define (stream-first s)
     (parse-stream-result s))
   (define (stream-rest s)
     (if (parse-stream-next-job s)
         (enter-the-parser/job (parse-stream-scheduler s)
                               (parse-stream-next-job s))
         empty-stream))])

(struct parse-failure ()
  #:transparent
  #:methods gen:stream
  [(define (stream-empty? s) #t)
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
  (port-broker requested-job job-info->job-cache)
  #:mutable
  #:transparent)
(define (make-scheduler port-broker)
  (scheduler port-broker #f (make-start-position-cache)))

(struct parser-job
  (
   ;; parser is either a parser struct or an alt-parser struct
   parser
   scheduler
   start-position
   result-index
   ;; gvector of siblings indexed by result-index
   siblings
   [port #:mutable]
   [continuation/worker #:mutable]
   ;; The actual result.
   [result #:mutable]
   ;; This one is not used for alt-parsers, but is used to keep track of the
   ;; stream from a procedure.
   [result-stream #:mutable]
   )
  #:transparent)


(struct cycle-breaker-job (failure-job cycle-jobs) #:transparent)

(define (job->result job)
  (match job
    [(cycle-breaker-job failure-job cycle-jobs)
     (parse-failure)]
    [(s/kw parser-job #:result r) r]))

(struct alt-worker
  (job remaining-jobs)
  #:mutable #:transparent)

(struct scheduled-continuation
  ;; The dependency is only mutated to break cycles.
  (job k [dependency #:mutable])
  #:transparent)


(define (job->scheduled-continuation j k dep)
  (scheduled-continuation j k dep #f))


(define (job->display job)
  (cond [(not job) #f]
        [(cycle-breaker-job? job) "cycle-breaker"]
        [else (format "~a@~a_~a"
                      (job->parser-name job)
                      (parser-job-start-position job)
                      (parser-job-result-index job))]))
(define (job->parser-name job)
  (define p (and job (parser-job-parser job)))
  (if (not p) p (parser-name p)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Caches

#|
Scheduler cache:
A weak hash port-broker->ephemeron with scheduler.
|#
(define the-scheduler-cache (make-weak-hasheq))
(define port-broker-scheduler
  (make-ephemeron-cache-lookup the-scheduler-cache make-scheduler))

#|
The job cache is a multi-level dictionary with the following keys / implementations:
* start-position / gvector
* parser / mutable hasheq
|#
(define (make-start-position-cache)
  (make-gvector #:capacity 10000))
(define (make-parser-cache)
  (make-hasheq))
(define (make-cp-params-cache)
  (make-hash))


(define (get-job-0! s parser cp-params start-position)
  (define (make-fresh-parser-job usable)
    (define siblings-vec (make-gvector))
    (define job (parser-job usable s start-position 0 siblings-vec #f #f #f #f))
    (gvector-add! siblings-vec job)
    job)
  (define (traverse-cache parser)
    ;; Traverse the cache.  If no job is found, it updates the cache with a job.
    ;; Either way it returns a job object.
    ;; Each level of the cache may have a literal job object or a layer of caching.
    ;; A layer of caching may be a gvector or a hash table.
    (define (job-match? j)
      (eq? parser (parser-job-parser j)))
    (define c/start-pos (scheduler-job-info->job-cache s))
    (define start-pos-referenced
      (and (< start-position (gvector-count c/start-pos))
           (gvector-ref c/start-pos start-position)))
    (define (start-pos-add-cache-layer! old-job)
      (define parser-cache-layer (make-parser-cache))
      (define new-job (make-fresh-parser-job parser))
      (hash-set! parser-cache-layer (parser-job-parser old-job) old-job)
      (hash-set! parser-cache-layer parser new-job)
      (gvector-set! c/start-pos start-position parser-cache-layer)
      new-job)
    (match start-pos-referenced
      [#f
       ;; extend gvector out to start position
       (for ([i (in-range (gvector-count c/start-pos)
                          (add1 start-position))])
         (gvector-add! c/start-pos #f))
       ;; add the parser to the cache
       (define new-job (make-fresh-parser-job parser))
       (gvector-set! c/start-pos start-position new-job)
       new-job]
      [(? parser-job?) (if (job-match? start-pos-referenced)
                           start-pos-referenced
                           (start-pos-add-cache-layer! start-pos-referenced))]
      [else
       (define c/parser start-pos-referenced)
       (define parser-referenced (hash-ref c/parser parser #f))
       (match parser-referenced
         [#f
          (define new-job (make-fresh-parser-job parser))
          (hash-set! c/parser parser new-job)
          new-job]
         [(? parser-job?) parser-referenced])]))

  (define usable (parser->usable parser))
  (traverse-cache usable))

(define (get-next-job! job)
  (match job
    [(s/kw parser-job #:parser parser #:scheduler scheduler #:cp-params cp-params
           #:start-position start-position #:result-index result-index
           #:siblings siblings #:port port)
     (define next-index (add1 result-index))
     (if (< next-index (gvector-count siblings))
         (gvector-ref siblings next-index)
         (let ([new-job (parser-job parser scheduler start-position
                                    next-index siblings port #f #f #f)])
           (gvector-add! siblings new-job)
           new-job))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Scheduling

(define chido-parse-prompt (make-continuation-prompt-tag 'chido-parse-prompt))
(define current-chido-parse-job (make-parameter #f))
(define recursive-enter-flag (gensym 'recursive-enter-flag))

(define (enter-the-parser port-broker parser start-position)
  (define s (port-broker-scheduler port-broker))
  (define cp-params (current-chido-parse-parameters))
  (define j (get-job-0! s parser cp-params start-position))
  (enter-the-parser/job s j))

(define (enter-the-parser/job scheduler job)
  (define ready-result (job->result job))
  (or ready-result
      (if (current-chido-parse-job)
          ;; This is a (potentially left-) recursive call.
          ;; So we de-schedule the current work by capturing its continuation,
          ;; we schedule its dependency, and then we abort back to the
          ;; scheduler loop.
          (let ([parent-job (current-chido-parse-job)])
            (call-with-composable-continuation
             (λ (k)
               (define sched-k (job->scheduled-continuation parent-job k job))
               (set-parser-job-continuation/worker! parent-job sched-k)
               (abort-current-continuation chido-parse-prompt #f))
             chido-parse-prompt))
          ;; This is the original entry into the parser machinery.
          (let ([result (begin
                          (set-scheduler-requested-job! scheduler job)
                          (run-scheduler scheduler))])
            (set-scheduler-requested-job! scheduler #f)
            result))))



(define (find-work s job-stack)
  ;; Returns a hint stack for an actionable job -- IE a list where the
  ;; first element is an actionable job, the second element is a job
  ;; that depends on the first, etc
  ;; OR
  ;; Returns #f if there is no actionable job.
  ;;
  ;; s is a scheduler
  ;; job-stacks is a list of these dependency stacks

  #|
  TODO - fail-chain-dependent!, do-alt-fail!, and anything about finding the last alt was an attempt at optimizing that wasn't very useful, so it's all commented out or unused.  Maybe I should delete it.  At any rate I want the code checked into my repo history.  Maybe later I'll realize that the code is useful but I did something that ruined it.  Or maybe I'll delete it later.  I don't know.
  |#
  #;(define (fail-chain-dependent! job stack)
    (define dep
      (match (parser-job-continuation/worker job)
        [(s/kw scheduled-continuation #:dependency dep) dep]
        [else (error
               'find-work
               "TODO - this shouldn't happen -- alt-job in fail-chain-dependent")]))
    (if (memq dep stack)
        (match (parser-job-continuation/worker dep)
          [(and sk (s/kw scheduled-continuation #:dependency dep-dep))
           (set-scheduled-continuation-dependency! sk
                                                   (cycle-breaker-job dep-dep))
           (set-scheduled-continuation-ready?! sk #t)]
          [(and aw (s/kw alt-worker))
           (set-alt-worker-remaining-jobs! aw '())])
        (fail-chain-dependent! dep (cons dep stack))))
  #;(define (do-alt-fail! alt-stack)
    (define last-alt (car alt-stack))
    (match (parser-job-continuation/worker last-alt)
      [(s/kw alt-worker #:remaining-jobs remaining-jobs)
       (for ([rj remaining-jobs])
         (fail-chain-dependent! rj alt-stack))
       (find-work s alt-stack)]))

  (inc-find-work!)
  (let loop ([stacks (list job-stack)]
             [blocked '()]
             [last-alt-stack #f #;(get-last-alt-stack job-stack)])
    (inc-find-work-loop!)
    (if (null? stacks)
        #f
        #;(if last-alt-stack
            (do-alt-fail! last-alt-stack)
            #f)
        (let* ([jstack (car stacks)]
               [j (car jstack)]
               #;[last-alt (and last-alt-stack (car last-alt-stack))]
               #;[alt-fail (and last-alt (not (memq last-alt jstack)))])
          (cond #;[alt-fail (do-alt-fail! last-alt-stack)]
                [(memq j blocked) (loop (cdr stacks) blocked last-alt-stack)]
                ;; if it has a continuation/worker, add dependencies to jobs
                [else (match (parser-job-continuation/worker j)
                        [(s/kw scheduled-continuation
                               #:dependency dependency
                               #:ready? ready?)
                         (cond [ready? jstack]
                               [else (loop (cons (cons dependency jstack)
                                                 (cdr stacks))
                                           (cons j blocked)
                                           last-alt-stack)])]
                        [(s/kw alt-worker
                               #:ready-jobs ready-jobs
                               #:remaining-jobs remaining-jobs)
                         (cond [(not (null? ready-jobs)) jstack]
                               [(null? remaining-jobs) jstack]
                               [else (loop (append (map (λ (rj) (cons rj jstack))
                                                        remaining-jobs)
                                                   (cdr stacks))
                                           (cons j blocked)
                                           jstack)])]
                        [#f jstack])])))))

(define (find-and-run-actionable-job s hint-stack using-hint?)
  (define (run-actionable-job? j)
    ;; TODO - this is probably not great, but for now I just want to get things working again...
    (if (job->result j)
        (run-scheduler s)
        (run-actionable-job s j)))
  (let* ([actionable-job-stack (find-work s hint-stack)]
         [actionable-job (and actionable-job-stack (car actionable-job-stack))])
    (when (not actionable-job) (inc-actionable-job-false!))
    (and actionable-job-stack (set-scheduler-hint-stack-stack!
                               s (cons actionable-job-stack
                                       (cdr (scheduler-hint-stack-stack s)))))
    (cond
      [(and actionable-job (job->result actionable-job))
       (pop-hint! s)
       (ready-dependents! actionable-job)
       (run-scheduler s)]
      [actionable-job (run-actionable-job? actionable-job)]
      [using-hint?
       ;; try to find work in another part of the work tree...
       ;; TODO - this should always be able to make progress, so the else case should not be necessary...
       #;(define new-work-stack
         (get-last-alt-stack (car (scheduler-hint-stack-stack s))))
       (set-scheduler-hint-stack-stack!
        s (cons #;new-work-stack '() (cdr (scheduler-hint-stack-stack s))))
       (run-scheduler s)]
      [else
       ;; TODO - This should be optimized so that if `find-work` also finds this cycle...
       (fail-smallest-cycle! s)
       (run-scheduler s)])))

(define (run-scheduler s)
  (define done-k (car (scheduler-done-k-stack s)))
  (define scheduler-done? (scheduled-continuation-ready? done-k))
  (cond
    [scheduler-done?
     ;; Escape the mad world of delimited-continuation-based parsing!
     (job->result (scheduled-continuation-dependency done-k))]
    [else
     (inc-run-scheduler!)
     (define using-hint? #t)
     (when (null? (car (scheduler-hint-stack-stack s)))
       (inc-no-hint!)
       (set! using-hint? #f)
       (push-hint! s (car (scheduler-top-job-stack s))))
     (define hint-stack (car (scheduler-hint-stack-stack s)))
     (define hint-worker (parser-job-continuation/worker (car hint-stack)))
     (define (run-actionable-job? j)
       ;; TODO - this is probably not great, but for now I just want to get things working again...
       (if (job->result j)
           (begin (pop-hint! s)
                  (run-scheduler s))
           (run-actionable-job s j)))
     (match hint-worker
       [#f (if (job->result (car hint-stack))
               (begin (pop-hint! s)
                      (run-scheduler s))
               (run-actionable-job? (car hint-stack)))]
       [(s/kw scheduled-continuation #:job job #:ready? #t)
        ;(pop-hint! s)
        (run-actionable-job? job)]
       [(s/kw scheduled-continuation)
        (find-and-run-actionable-job s hint-stack using-hint?)]
       [(s/kw alt-worker #:job job #:ready-jobs (list ready-job rjs ...))
        (run-actionable-job? job)]
       [(s/kw alt-worker #:job job #:remaining-jobs (list))
        (run-actionable-job? job)]
       [(s/kw alt-worker)
        ;; No ready jobs
        (find-and-run-actionable-job s hint-stack using-hint?)]
       )]))


(define (fail-smallest-cycle! scheduler)
  #|
  TODO - better name.
  This isn't failing the smallest cycle necessarily, but it is failing the job in a cycle that first depends back on something earlier in the chain to the root goal.
  |#
  (define (rec goal jobs)
    (match goal
      [(s/kw alt-worker #:job job #:remaining-jobs remaining-jobs)
       ;; If I'm in a situation to have to break a cycle, all remaining-jobs
       ;; are cyclic.  So let's just break the first one for now.
       (define next-job (car remaining-jobs))
       (rec (parser-job-continuation/worker next-job) (cons job jobs))]
      [(s/kw scheduled-continuation #:job job #:dependency dependency)
       (if (memq dependency jobs)
           (begin
             (set-scheduled-continuation-dependency!
              goal
              (cycle-breaker-job dependency jobs))
             (set-scheduled-continuation-ready?! goal #t))
           (rec (parser-job-continuation/worker dependency)
                (cons job jobs)))]))
  (rec (car (scheduler-done-k-stack scheduler)) '()))

(define (prefix-fail! scheduler job)
  (match job
    [(s/kw parser-job #:parser p #:start-position pos #:result #f)
     (define pb (scheduler-port-broker scheduler))
     (cache-result-and-ready-dependents!
      scheduler
      job
      (parse-failure p
                     (format "prefix didn't match: ~v" (parser-prefix p))
                     (port-broker-line pb pos)
                     (port-broker-column pb pos)
                     pos pos pos
                     #f #f #f))]
    [else (void)]))

(define (string-job-finalize! scheduler job match?)
  (match job
    [(s/kw parser-job #:parser s #:start-position start-position)
     (define result
       (parameterize ([current-chido-parse-job job])
         (if match?
             (parse-stream
              (make-parse-derivation s #:end (+ start-position
                                                (string-length s)))
              #f
              scheduler)
             (make-parse-failure #:message (format "literal didn't match: ~s" s)
                                 #:position start-position))))
     (cache-result-and-ready-dependents! scheduler job result)]))

(define (run-actionable-job scheduler job)
  ;(eprintf "running actionable job: ~a\n" (job->display job))
  (when (job->result job)
    (error 'chido-parse
           "internal error - run-actionable-job got a job that was already done: ~a"
           (job->display job)))
  (match job
    [(s/kw parser-job #:parser parser #:continuation/worker k/worker
           #:result-stream result-stream #:result-index result-index
           #:start-position start-position #:cp-params cp-params)
     (match k/worker
       [(s/kw scheduled-continuation #:job job #:dependency dependency #:k k)
        (do-run! scheduler
                 k
                 job
                 #t
                 (job->result dependency))]
       [(s/kw alt-worker #:job job
              #:ready-jobs (list ready-job rjs ...)
              #:remaining-jobs remaining-jobs
              #:failures failures)
        (set-alt-worker-remaining-jobs! k/worker (remq ready-job remaining-jobs))
        (set-alt-worker-ready-jobs! k/worker rjs)
        (define result (job->result ready-job))
        (cond
          [(parse-failure? result)
           (set-alt-worker-failures! k/worker (cons result failures))]
          [(parse-stream? result)
           (let ([result-contents (stream-first result)]
                 [this-next-job (get-next-job! job)]
                 [dep-next-job (get-next-job! ready-job)])
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
             (when (job->result dep-next-job)
               (set-alt-worker-ready-jobs!
                k/worker
                (cons dep-next-job (alt-worker-ready-jobs k/worker)))))]
          [else
           (error 'chido-parse
                  "Internal error - alt-worker got a non-stream result: ~s"
                  result)])
        (run-scheduler scheduler)]
       [(s/kw alt-worker #:job job
              #:ready-jobs (list)
              #:remaining-jobs (list))
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
                        job
                        #f #f)]
              [(equal? 0 result-index)
               (match parser
                 [(s/kw proc-parser
                        #:name name
                        #:prefix prefix
                        #:procedure procedure
                        #:preserve-prefix? preserve-prefix?
                        #:use-port? use-port?)
                  (define port-broker (scheduler-port-broker scheduler))
                  (define proc-start-position
                    (if preserve-prefix?
                        start-position
                        (+ start-position (string-length prefix))))
                  (define proc-input (if use-port?
                                         (port-broker->port port-broker
                                                            proc-start-position)
                                         (port-broker-wrap port-broker
                                                           proc-start-position)))
                  (when use-port?
                    (set-parser-job-port! job proc-input))
                  ;; TODO - optimize this peek for alt parsers, at least...
                  (if (port-broker-substring? port-broker start-position prefix)
                      (do-run! scheduler
                               (λ ()
                                 (parameterize ([current-chido-parse-parameters
                                                 cp-params])
                                   (procedure proc-input)))
                               job
                               #f #f)
                      (begin (prefix-fail! scheduler job)
                             (run-scheduler scheduler)))]
                 [(s/kw alt-parser #:trie trie #:parsers parsers)
                  (define (mk-dep p)
                    (define j (get-job-0! scheduler p cp-params start-position))
                    (when (string? p) (string-job-finalize! scheduler j #t))
                    j)
                  (define pb (scheduler-port-broker scheduler))
                  (define deps-with-matched-prefixes
                    (let loop ([matches '()]
                               [t trie]
                               [pos start-position])
                      (define new-matches (append (trie-values t) matches))
                      (define new-trie (trie-step t (port-broker-char pb pos) #f))
                      (if new-trie
                          (loop new-matches new-trie (add1 pos))
                          new-matches)))
                  ;; We could fail all parses here, but if we just ignore them
                  ;; instead it is significantly faster.
                  #;(for ([p parsers])
                      (when (not (memq p deps-with-matched-prefixes))
                        (prefix-fail! scheduler (mk-dep p))))
                  (define dep-jobs (map mk-dep deps-with-matched-prefixes))
                  (define ready-deps (filter parser-job-result dep-jobs))
                  (define unready-deps
                    (filter (λ (x) (not (parser-job-result x))) dep-jobs))
                  (define worker
                    (alt-worker job unready-deps ready-deps '() #f))
                  (for ([dep unready-deps])
                    (push-parser-job-dependent! dep worker))
                  (set-parser-job-continuation/worker! job worker)
                  (cond [(not (null? ready-deps))
                         ;; There is a result ready, so we just run this same job again to set the result.
                         (run-actionable-job scheduler job)]
                        [(and (not (null? unready-deps))
                              (null? (cdr unready-deps))
                              (job-immediately-actionable? (car unready-deps)))
                         (run-actionable-job scheduler (car unready-deps))]
                        [else (run-scheduler scheduler)])
                  ;(push-hint! scheduler worker)
                  ]
                 [(? string?)
                  (define s parser)
                  (define pb (scheduler-port-broker scheduler))
                  (define match?
                    (port-broker-substring? pb start-position s))
                  (string-job-finalize! scheduler job match?)
                  (run-scheduler scheduler)])]
              [else
               ;; In this case there has been a result stream but it is dried up.
               (define pb (scheduler-port-broker scheduler))
               (define pos (parser-job-start-position job))
               (let ([end-failure (parse-failure (parser-job-parser job)
                                                 "No more results"
                                                 (port-broker-line pb pos)
                                                 (port-broker-column pb pos)
                                                 pos pos pos
                                                 #f #f #f)])
                 (cache-result-and-ready-dependents! scheduler
                                                     job
                                                     end-failure)
                 (set-parser-job-continuation/worker! job #f)
                 (run-scheduler scheduler))])])]))

(define (do-run! scheduler thunk/k job continuation-run? k-arg)
  (when (not job)
    (error 'chido-parse "Internal error - trying to recur with no job"))
  #|
  When continuation-run? is true, we are running a continuation (instead of a fresh thunk) and we want to supply k-arg.
  This keeps us from growing the continuation at all when recurring.
  |#
  (define (result-loop new-thunk)
    (if new-thunk
        (call-with-continuation-prompt new-thunk chido-parse-prompt result-loop)
        recursive-enter-flag))
  (define result
    (if continuation-run?
        (call-with-continuation-prompt thunk/k
                                       chido-parse-prompt
                                       result-loop
                                       k-arg)
        (result-loop (λ ()
                       (parameterize ([current-chido-parse-job job])
                         (call-with-continuation-prompt
                          thunk/k
                          parse*-direct-prompt))))))
  (let flatten-loop ([result result])
    (if (eq? result recursive-enter-flag)
        (run-scheduler scheduler)
        (if (and (stream? result)
                 (not (flattened-stream? result))
                 (not (parse-failure? result)))
            (flatten-loop
             (call-with-continuation-prompt
              (λ () (parameterize ([current-chido-parse-job job])
                      (delimit-parse*-direct
                       (stream-flatten result))))
              chido-parse-prompt
              result-loop))
            (begin (cache-result-and-ready-dependents! scheduler job result)
                   (run-scheduler scheduler))))))

(define (ready-dependents! job)
  (for ([dep (parser-job-dependents job)])
    (match dep
      [(s/kw alt-worker #:ready-jobs ready-jobs)
       (set-alt-worker-ready-jobs! dep (cons job ready-jobs))]
      [(s/kw scheduled-continuation)
       (set-scheduled-continuation-ready?! dep #t)]))
  (set-parser-job-dependents! job '()))

(define (cache-result-and-ready-dependents! scheduler job result)
  (define p (parser-job-parser job))
  (cond [(proc-parser? p)
         (cache-result-and-ready-dependents!/procedure-job scheduler job result)]
        [else
         (cache-result-and-ready-dependents!/builtin scheduler job result)]))

(define (cache-result-and-ready-dependents!/builtin scheduler job result)
  ;; This version only gets pre-sanitized results, and is straightforward.
  (when (and (not (parse-failure? result))
             (not (parse-stream? result)))
    (error 'chido-parse-cache-result-and-ready-dependents/builtin!
           "trying to cache something for job ~s with a bad type: ~s\n"
           (job->display job)
           result))
  (set-parser-job-result! job result)
  (ready-dependents! job))

(define (cache-result-and-ready-dependents!/procedure-job
         scheduler job result)
  ;; Clear the result-stream, if there is one, because it's not needed anymore.
  (set-parser-job-result-stream! job #f)
  (match result
    [(s/kw parse-failure)
     (set-parser-job-result! job result)
     (ready-dependents! job)]
    [(? (λ (x) (and (stream? x) (stream-empty? x))))
     ;; Turn it into a parse failure and recur.
     (match job
       [(s/kw parser-job #:parser parser #:start-position start-position)
        (match parser
          [(s/kw proc-parser #:name name)
           (define failure
             (let ([inner-failures (and (flattened-stream? result)
                                        (flattened-stream-failures result))])
               (match inner-failures
                 [(or #f (list))
                  (define pb (scheduler-port-broker scheduler))
                  (parse-failure parser
                                 "parse returned empty stream"
                                 (port-broker-line pb start-position)
                                 (port-broker-column pb start-position)
                                 start-position start-position start-position
                                 #f #f #f)]
                 [(list one-fail) one-fail]
                 [(list fail ...)
                  (parameterize ([current-chido-parse-job job])
                    (make-parse-failure #:failures fail))])))
           (cache-result-and-ready-dependents!/procedure-job
            scheduler
            job
            failure)])])]
    [(? stream?)
     ;; Recur with stream-first, setting the stream as the result-stream
     (define next-job (get-next-job! job))
     (set-parser-job-result-stream! next-job result)
     (cache-result-and-ready-dependents!/procedure-job
      scheduler job (reject-raw-results job (stream-first result)))]
    [(s/kw parse-derivation)
     (define next-job (get-next-job! job))
     (define wrapped-result
       (parse-stream result next-job scheduler))
     (set-parser-job-result! job wrapped-result)
     (ready-dependents! job)]
    [else (cache-result-and-ready-dependents! scheduler
                                              job
                                              (reject-raw-results
                                               job
                                               result))]))

(define (reject-raw-results job result)
  (cond
    [(parse-derivation? result) result]
    [(and (not (parse-derivation? result))
          (not (stream? result))
          (not (procedure? result))
          (parser-job-port job))
     (let ([port-broker (scheduler-port-broker (parser-job-scheduler job))]
           [start-position (parser-job-start-position job)])
       (parse-derivation
        result
        #t ;; forced?
        (parser-job-parser job)
        (port-broker-source-name port-broker)
        (port-broker-line port-broker
                          start-position)
        (port-broker-column port-broker
                            start-position)
        start-position
        (port->pos (parser-job-port job))
        '()))]
    [else (error 'chido-parse "job ~a returned non-derivation: ~v"
                 (job->display job) result)]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Parsing outer API
(define (port->pos p)
  (let-values ([(line col pos) (port-next-location p)]) pos))

(define (parse-inner core-proc port/pbw parser start)
  (define pb (if (port-broker-wrap? port/pbw)
                 (port-broker-wrap-broker port/pbw)
                 (or (port->port-broker port/pbw)
                     (port-broker port/pbw))))
  (define start-pos (match start
                      [(? number?) start]
                      [(? parse-derivation?) (parse-derivation-end-position start)]
                      [#f (if (port-broker-wrap? port/pbw)
                              (port-broker-wrap-position port/pbw)
                              (port->pos port/pbw))]))
  (core-proc pb parser start-pos))

(define (parse* port/pbw parser
                #:start [start #f])
  (parse-inner enter-the-parser
               (cond [(string? port/pbw) (open-input-string port/pbw)]
                     [(input-port? port/pbw) port/pbw]
                     [(port-broker-wrap? port/pbw) port/pbw]
                     [else (error 'parse* "bad input: ~v" port/pbw)])
               parser
               start))

(define-syntax (for/parse stx)
  (syntax-parse stx
    [(_ ([arg-name input-stream]
         (~optional (~seq #:failure failure-arg:expr)))
        body ...+)
     #'(for/parse-proc (λ (arg-name) body ...)
                       (λ () input-stream)
                       (~? failure-arg
                           (λ (f) f (make-parse-failure #:inner-failure f))))]))

(define (parse*-direct port parser
                      #:start [start #f]
                      #:failure [failure-arg #f])
  (when (not (input-port? port))
    (error 'parse*-direct "parse*-direct requires a port as an argument, given: ~v"
           port))
  (define start-use (or start (port->pos port)))
  ;; This one lets the user get a single result back, but the return is actually a stream.
  (define (direct-recursive-parse-core pb parser core-start)
    (define job (current-chido-parse-job))
    (define params (current-chido-parse-parameters))
    (define new-derivation
      (call-with-composable-continuation
       (λ (k)
         (abort-current-continuation
          parse*-direct-prompt
          ;; TODO - better failure handling and propagation
          (λ () (for/parse ([d (parse* port parser #:start core-start)]
                            #:failure (λ (f)
                                        (parameterize
                                            ([current-chido-parse-job job]
                                             [current-chido-parse-parameters params])
                                          (or (and failure-arg
                                                   (failure-arg f))
                                              (make-parse-failure
                                               #:inner-failure f)))))
                           (k d)))))
       parse*-direct-prompt))
    (define new-pos (parse-derivation-end-position new-derivation))
    (port-broker-port-reset-position! port new-pos)
    new-derivation)
  (parse-inner direct-recursive-parse-core port parser start-use))



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
        (make-parse-failure #:message "Didn't match.")))
  (define a1-parser-obj (proc-parser "a" a1-parser-proc))

  (define (Aa-parser-proc port)
    (for/parse
     ([d/A (parse* port (get-A-parser))])
     (for/parse
      ([d/a (parse* port a1-parser-obj
                    #:start d/A)])
      (make-parse-derivation (string-append (parse-derivation-result d/A)
                                            (parse-derivation-result d/a))
                             #:derivations (list d/A d/a)))))

  (define Aa-parser-obj (proc-parser "Aa" Aa-parser-proc))


  (define A-parser (alt-parser "A"
                               (list
                                Aa-parser-obj
                                a1-parser-obj)))
  (define (get-A-parser) A-parser)

  (define results1 (parse* p1 A-parser))
  (check-equal? (map parse-derivation-result (stream->list results1))
                (list "a" "aa" "aaa" "aaaa" "aaaaa"))


  (define (Aa-parser-proc/direct port)
    (define d/A (parse*-direct port (get-A-parser/direct)))
    (define d/a (parse*-direct port a1-parser-obj))
    (make-parse-derivation (string-append (parse-derivation-result d/A)
                                          (parse-derivation-result d/a))
                           #:derivations (list d/A d/a)))
  (define Aa-parser-obj/direct
    (proc-parser "Aa" Aa-parser-proc/direct))
  (define A-parser/direct (alt-parser "A"
                                      (list
                                       Aa-parser-obj/direct
                                       a1-parser-obj)))
  (define (get-A-parser/direct) A-parser/direct)

  (define results2 (parse* p1 A-parser/direct))
  (check-equal? (map parse-derivation-result (stream->list results2))
                (list "a" "aa" "aaa" "aaaa" "aaaaa"))

  (define c3-parser
    (proc-parser "c3-parser" (λ (port) (read-string 3 port))))
  (check-equal?
   (map parse-derivation-result
        (stream->list
         (parse* (open-input-string "abc")
                 c3-parser)))
   '("abc"))

  (check-equal?
   (map parse-derivation-result
        (stream->list
         (parse* (open-input-string "◊")
                 "◊")))
   (list "◊"))

  )

