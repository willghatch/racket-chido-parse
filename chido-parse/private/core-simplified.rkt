#lang racket/base

#|
Simplifications from the full core.rkt:
* Error handling - there is only one error object which contains no useful data
* derivation objects - results are strict instead of potentially lazy, only start/end positions are tracked (no line/column/span/source-name)
* proc-parsers and alt-parsers have no extra metadata like prefix, nullability, promise-no-left-recursion, prefix tries, etc
* no custom parser structs or raw strings (but still supporting parser thunks because they are useful to “tie the knot”)
* the scheduler always captures continuations, not checking whether left recursion is possible
* jobs don't track their dependents to update them automatically - we just do a search for available work each time we enter the scheduler
* no chido-parse-parameters

* TODO - maybe switch to string input instead of ports (needs a change from parsers being (-> port derivation) to (-> string position derivation))
* TODO - maybe simplify the caching -- eg. sibling jobs are stored in a vector in the job record itself
|#


(require
 "port-broker.rkt"
 "ephemeron-cache.rkt"
 "stream-flatten.rkt"
 racket/stream
 racket/match
 data/gvector
 (rename-in kw-make-struct
            [make/kw s/kw])
 (for-syntax
  racket/base
  syntax/parse
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

;; For finding the end point of a derivation when one is not explicitly provided
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


(struct scheduler
  (port-broker [requested-job #:mutable] job-cache)
  #:transparent)
(define (make-scheduler port-broker)
  (scheduler port-broker #f (make-job-cache)))

(struct parser-job
  (parser
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

(define (make-scheduled-continuation-for-job j k dep)
  (scheduled-continuation j k dep))

;; In this simplified version, parser names are solely for ease of debugging this implementation itself.
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

;; Scheduler cache (IE global cache containing scheduler objects):
;; A weak hash port-broker->ephemeron with scheduler.
(define the-scheduler-cache (make-weak-hasheq))
(define port-broker-scheduler
  (make-ephemeron-cache-lookup the-scheduler-cache make-scheduler))

;; The job cache is a hash table mapping (list parser start-position) to job-0s.
;; Jobs contain a (mutable) vector of their siblings.
(define (make-job-cache) (make-hash))

(define (get-job-0! s parser start-position)
  (define usable (parser->usable parser))
  (define cache (scheduler-job-cache s))
  (define key (list usable start-position))
  (define j-maybe (hash-ref cache key #f))
  (or j-maybe
      (let* ([siblings-vec (make-gvector)]
             [fresh-job (parser-job usable s start-position 0
                                    siblings-vec #f #f #f #f)])
        (gvector-add! siblings-vec fresh-job)
        (hash-set! cache key fresh-job)
        fresh-job)))

(define (get-next-job! job)
  (match job
    [(s/kw parser-job #:parser parser #:scheduler scheduler
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
  (define j (get-job-0! s parser start-position))
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
               (define sched-k (make-scheduled-continuation-for-job parent-job k job))
               (set-parser-job-continuation/worker! parent-job sched-k)
               (abort-current-continuation chido-parse-prompt #f))
             chido-parse-prompt))
          ;; This is the original entry into the parser machinery.
          (let ([result (begin
                          (set-scheduler-requested-job! scheduler job)
                          (run-scheduler scheduler))])
            (set-scheduler-requested-job! scheduler #f)
            result))))

(define (run-scheduler s)
  (define orig-job (scheduler-requested-job s))
  (define done?/result (job->result orig-job))
  (cond
    [done?/result
     ;; Escape the mad world of delimited-continuation-based parsing!
     done?/result]
    [else
     (define next-job (find-work s '() (list orig-job)))
     (if (not next-job)
         (begin (fail-smallest-cycle! s) (run-scheduler s))
         (schedule-job! s next-job))]))

(define (find-work s blocked-jobs jobs-to-check)
  ;; Returns #f if no work is found
  (and (not (null? jobs-to-check))
       (let* ([j (car jobs-to-check)])
         (match (parser-job-continuation/worker j)
           [(s/kw scheduled-continuation #:dependency dependency)
            (cond [(job->result dependency) j]
                  [else
                   (define new-blocked (cons j blocked-jobs))
                   (define new-to-check (if (memq dependency new-blocked)
                                            (cdr jobs-to-check)
                                            (cons dependency (cdr jobs-to-check))))
                   (find-work s new-blocked new-to-check)])]
           [(s/kw alt-worker #:remaining-jobs remaining-jobs)
            (cond [(null? remaining-jobs) j]
                  [(findf job->result remaining-jobs) j]
                  [else
                   (define new-blocked (cons j blocked-jobs))
                   (define add-to-check (filter (λ (x) (not (memq x new-blocked)))
                                                remaining-jobs))
                   (define new-to-check (append add-to-check (cdr jobs-to-check)))
                   (find-work s new-blocked new-to-check)])]
           [#f j]))))

(define (fail-smallest-cycle! scheduler)
  ;; TODO - better name.
  ;; This isn't failing the smallest cycle necessarily, but it is failing the job in a cycle that first depends back on something earlier in the chain to the root goal.
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
              (cycle-breaker-job dependency jobs)))
           (rec (parser-job-continuation/worker dependency)
                (cons job jobs)))]))
  (rec (parser-job-continuation/worker (scheduler-requested-job scheduler)) '()))

(define (schedule-job! scheduler job)
  (when (job->result job)
    (error 'chido-parse
           "internal error - schedule-job! got a job that was already done: ~a"
           (job->display job)))
  (match job
    [(s/kw parser-job #:parser parser #:continuation/worker k/worker
           #:result-stream result-stream #:result-index result-index
           #:start-position start-position)
     (match k/worker
       [(s/kw scheduled-continuation #:job job #:dependency dependency #:k k)
        (do-run! scheduler k job #t (job->result dependency))]
       [(s/kw alt-worker #:job job #:remaining-jobs (list))
        ;; Finished alt-worker.
        (cache-result! scheduler job (parse-failure))
        (run-scheduler scheduler)]
       [(s/kw alt-worker #:job job #:remaining-jobs remaining-jobs)
        (define inner-ready-job (findf job->result remaining-jobs))
        (when (not inner-ready-job)
          (error 'schedule-job! "scheduled an alt-job that wasn't ready: ~a" (job->display job)))
        (define result (job->result inner-ready-job))
        (define other-remaining-jobs (remq inner-ready-job remaining-jobs))
        (define new-remaining-jobs
          (if (parse-failure? result)
              other-remaining-jobs
              (cons (get-next-job! inner-ready-job)
                    other-remaining-jobs)))
        (set-alt-worker-remaining-jobs! k/worker new-remaining-jobs)
        (match result
          [(? parse-failure?) (void)]
          [(? parse-stream?)
           (let ([result-contents (stream-first result)]
                 [this-next-job (get-next-job! job)])
             (define result-stream
               (parse-stream result-contents this-next-job scheduler))
             (cache-result! scheduler job result-stream)
             (set-alt-worker-job! k/worker this-next-job)
             (set-parser-job-continuation/worker! this-next-job k/worker)
             (set-parser-job-continuation/worker! job #f))])
        (run-scheduler scheduler)]
       [#f
        ;; no k/worker case
        (cond
          [(stream? result-stream)
           ;; IE the case of a procedural parser with a result stream to force
           (do-run! scheduler
                    (λ () (stream-rest result-stream))
                    job
                    #f #f)]
          [(equal? 0 result-index)
           ;; IE the first run of a parser
           (match parser
             [(s/kw proc-parser #:procedure procedure)
              (define proc-input (port-broker->port (scheduler-port-broker scheduler)
                                                    start-position))
              (set-parser-job-port! job proc-input)
              (do-run! scheduler
                       (λ () (procedure proc-input))
                       job
                       #f #f)]
             [(s/kw alt-parser #:parsers parsers)
              (define (mk-dep p)
                (get-job-0! scheduler p start-position))
              (define pb (scheduler-port-broker scheduler))
              (define dep-jobs (map mk-dep parsers))
              (define worker (alt-worker job dep-jobs))
              (set-parser-job-continuation/worker! job worker)
              (run-scheduler scheduler)])]
          [else
           ;; In this case there has been a result stream but it is dried up.
           (cache-result! scheduler job (parse-failure))
           (set-parser-job-continuation/worker! job #f)
           (run-scheduler scheduler)])])]))

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
            (begin (cache-result! scheduler job result)
                   (run-scheduler scheduler))))))

(define (cache-result! scheduler job result)
  (if (proc-parser? (parser-job-parser job))
      (cache-result!/procedure-job scheduler job result)
      ;; Alt-parsers get pre-sanitized results
      (set-parser-job-result! job result)))

(define (cache-result!/procedure-job scheduler job result)
  ;; Clear the result-stream, if there is one, because it's not needed anymore.
  (set-parser-job-result-stream! job #f)
  (match result
    [(s/kw parse-failure)
     (set-parser-job-result! job result)]
    [(? (λ (x) (and (stream? x) (stream-empty? x))))
     (set-parser-job-result! job (parse-failure))]
    [(? stream?)
     ;; Recur with stream-first, setting the stream as the result-stream.
     ;; Note that because we have already flattened result streams, stream-first
     ;; will never itself return a stream.
     (define next-job (get-next-job! job))
     (set-parser-job-result-stream! next-job result)
     (cache-result!/procedure-job scheduler job (stream-first result))]
    [(s/kw parse-derivation)
     (define next-job (get-next-job! job))
     (define wrapped-result
       (parse-stream result next-job scheduler))
     (set-parser-job-result! job wrapped-result)]
    [else (error 'chido-parse "job ~a returned non-derivation: ~v"
                 (job->display job) result)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Parsing outer API
(define (port->pos p)
  (let-values ([(line col pos) (port-next-location p)]) pos))

(define (parse-inner core-proc port/pbw parser start)
  (define pb (or (port->port-broker port/pbw)
                 (port-broker port/pbw)))
  (define start-pos (match start
                      [(? number?) start]
                      [(? parse-derivation?) (parse-derivation-end-position start)]
                      [#f (port->pos port/pbw)]))
  (core-proc pb parser start-pos))

(define (parse* port/pbw parser
                #:start [start #f])
  (parse-inner enter-the-parser
               (cond [(string? port/pbw) (open-input-string port/pbw)]
                     [(input-port? port/pbw) port/pbw]
                     [else (error 'parse* "bad input: ~v" port/pbw)])
               parser
               start))

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
    (define new-derivation
      (call-with-composable-continuation
       (λ (k)
         (abort-current-continuation
          parse*-direct-prompt
          ;; TODO - better failure handling and propagation
          (λ () (for/parse ([d (parse* port parser #:start core-start)])
                           (k d)))))
       parse*-direct-prompt))
    (define new-pos (parse-derivation-end-position new-derivation))
    (port-broker-port-reset-position! port new-pos)
    new-derivation)
  (parse-inner direct-recursive-parse-core port parser start-use))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; for/parse stuff
;;;; I'm putting this stuff inline because not supporting chido-parse-parameters simplifies the implementation of for/parse.

#|
The parse*-direct function needs its own continuation prompt.  When called during evaluation of a stream, it should NOT abort the processing of that stream to return its own stream instead.  The parse*-direct stream should be a child of any outer streams that are being processed.  Also, not supporting good failure objects simplifies it as well.
|#
(define parse*-direct-prompt (make-continuation-prompt-tag 'parse*-direct-prompt))

(define-syntax (delimit-parse*-direct stx)
  (syntax-parse stx
    [(_ e:expr)
     #'(call-with-continuation-prompt
        (λ () e)
        parse*-direct-prompt)]))


(define (for/parse-proc body-proc arg-stream-thunk)
  (let loop ([stream (arg-stream-thunk)])
    (cond [(stream-empty? stream) stream]
          [else (let ([v1 (stream-first stream)])
                  (stream-cons (body-proc v1)
                               (loop (stream-rest stream))))])))

(define-syntax (for/parse stx)
  (syntax-parse stx
    [(_ ([arg-name input-stream])
        body ...+)
     #'(for/parse-proc (λ (arg-name) body ...)
                       (λ () input-stream))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Tests

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
        (parse-failure)))
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
  )
