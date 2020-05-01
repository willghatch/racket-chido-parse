#lang racket/base
#|
Simplifications from the full core.rkt:
* Error handling - parse failure objects are replaced by empty-stream objects with no interesting data.
* derivation objects - results are strict instead of potentially lazy, only start/end positions are tracked (no line/column/span/source-name)
* proc-parsers and alt-parsers have no extra metadata like prefix, nullability, promise-no-left-recursion, prefix tries, etc
* no custom parser structs or raw strings (but still supporting parser thunks because they are useful to “tie the knot”)
* the scheduler always captures continuations, not checking whether left recursion is possible
* jobs don't track their dependents to update them automatically - we just do a search for available work each time we enter the scheduler
* no chido-parse-parameters
* input is string-based instead of port-based -- note that this means parse functions are (-> string position stream-tree) instead of (-> port stream-tree)
* caching is simpler
|#

(require racket/match racket/stream "stream-flatten.rkt")

(struct scheduler (input-string [requested-job #:mutable] job-cache))
(define (make-scheduler input-string)
  (scheduler input-string #f (make-job-cache)))

(struct proc-parser (procedure))
(struct alt-parser (parsers))
;; We also allow (-> parser) thunks as parsers, which is helpful to tie
;; the knot for recursive references.
(define parser-cache (make-weak-hasheq))
(define (parser->usable p)
  (match p
    [(or (? proc-parser?) (? alt-parser?)) p]
    [(? procedure?)
     (hash-ref parser-cache p (λ () (let ([result (parser->usable (p))])
                                      (hash-set! parser-cache p result)
                                      result)))]))

(struct parser-job
  (parser start-position result-index
   [continuation/worker #:mutable] [result #:mutable] [result-stream #:mutable]))
(define cycle-breaker-job (parser-job #f #f #f #f empty-stream #f))
(struct alt-worker (job remaining-jobs) #:mutable)
(struct scheduled-continuation
  ;; The dependency is only mutated to break cycles.
  (job k [dependency #:mutable]))
(define current-chido-parse-job (make-parameter #f))

(struct parse-derivation (result parser start-position end-position derivation-list))
(define (make-parse-derivation result derivations end)
  (match (current-chido-parse-job)
    [(parser-job parser start-position _ _ _ _)
     (parse-derivation result parser start-position end derivations)]
    [else (error 'make-parse-derivation
                 "Not called during the dynamic extent of chido-parse...")]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Caches

(define string->scheduler-cache (make-weak-hash))
(define (string->scheduler str)
  (hash-ref string->scheduler-cache str (λ ()
                                          (define s (make-scheduler str))
                                          (hash-set! string->scheduler-cache str s)
                                          s)))

;; The job cache is a hash table mapping (list parser start-position index) to jobs.
(define (make-job-cache) (make-hash))
(define (get-job s parser start-position result-index)
  (define usable (parser->usable parser))
  (define cache (scheduler-job-cache s))
  (define key (list usable start-position result-index))
  (hash-ref cache key (λ () (let* ([fresh-job (parser-job usable start-position
                                                          result-index #f #f #f)])
                              (hash-set! cache key fresh-job)
                              fresh-job))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Parsing outer API

(define (parse* in-string parser pos)
  (define scheduler (string->scheduler in-string))
  (enter-the-parser scheduler (get-job scheduler parser pos 0)))

(define (parse*-direct in-string parser pos)
  (call-with-composable-continuation
   (λ (k)
     (abort-current-continuation
      parse*-direct-prompt
      (λ () (for/parse ([d (parse* in-string parser pos)])
                       (k d)))))
   parse*-direct-prompt))

(define-syntax-rule (for/parse ([arg-name input-stream]) body)
  (let loop ([stream input-stream])
    (cond [(stream-empty? stream) stream]
          [else (let ([arg-name (stream-first stream)])
                  (stream-cons body
                               (loop (stream-rest stream))))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Parsing

(define (enter-the-parser scheduler job)
  (or (parser-job-result job)
      (if (current-chido-parse-job)
          ;; This is a (potentially left-) recursive call.
          ;; So we de-schedule the current work by capturing its continuation,
          ;; and then we abort back to the scheduler.
          (let ([parent-job (current-chido-parse-job)])
            (call-with-composable-continuation
             (λ (k)
               (set-parser-job-continuation/worker!
                parent-job (scheduled-continuation parent-job k job))
               (abort-current-continuation chido-parse-prompt #f))
             chido-parse-prompt))
          ;; This is the original entry into the parser machinery.
          (let ([result (begin (set-scheduler-requested-job! scheduler job)
                               (run-scheduler scheduler))])
            (set-scheduler-requested-job! scheduler #f)
            result))))

(define (run-scheduler s)
  (or (parser-job-result (scheduler-requested-job s))
      (let ([next-job (find-work s '() (list (scheduler-requested-job s)))])
        (if (not next-job)
            (begin (fail-cycle! s) (run-scheduler s))
            (schedule-job! s next-job)))))

(define (find-work s blocked-jobs to-check)
  ;; Returns #f if no work is found
  (and (not (null? to-check))
       (let* ([j (car to-check)])
         (match (parser-job-continuation/worker j)
           [(scheduled-continuation job k dependency)
            (cond [(parser-job-result dependency) j]
                  [else (define new-blocked (cons j blocked-jobs))
                        (define new-to-check (if (memq dependency new-blocked)
                                                 (cdr to-check)
                                                 (cons dependency (cdr to-check))))
                        (find-work s new-blocked new-to-check)])]
           [(alt-worker _ remaining-jobs)
            (cond [(null? remaining-jobs) j]
                  [(findf parser-job-result remaining-jobs) j]
                  [else (define new-blocked (cons j blocked-jobs))
                        (define add-to-check
                          (filter (λ (x) (not (memq x new-blocked))) remaining-jobs))
                        (define new-to-check (append add-to-check (cdr to-check)))
                        (find-work s new-blocked new-to-check)])]
           [#f j]))))

(define (fail-cycle! scheduler)
  ;; Fail the first job that is found to depend back on something earlier in
  ;; the chain back to the root goal.
  (define (rec goal jobs)
    (match goal
      [(alt-worker job remaining-jobs)
       (define next-job (car remaining-jobs))
       (rec (parser-job-continuation/worker next-job) (cons job jobs))]
      [(scheduled-continuation job k dependency)
       (if (memq dependency jobs)
           (set-scheduled-continuation-dependency! goal cycle-breaker-job)
           (rec (parser-job-continuation/worker dependency)
                (cons job jobs)))]))
  (rec (parser-job-continuation/worker (scheduler-requested-job scheduler)) '()))

(define (schedule-job! scheduler job)
  (match job
    [(parser-job parser pos result-index k/worker result result-stream)
     (match k/worker
       [(scheduled-continuation job k dependency)
        (do-run! scheduler k job #t (parser-job-result dependency))]
       [(alt-worker job (list))
        ;; Finished alt-worker.
        (cache-result! scheduler job empty-stream)
        (run-scheduler scheduler)]
       [(alt-worker job remaining-jobs)
        (define inner-ready-job (findf parser-job-result remaining-jobs))
        (define result (parser-job-result inner-ready-job))
        (define other-remaining-jobs (remq inner-ready-job remaining-jobs))
        (define new-remaining-jobs
          (if (parse-failure? result)
              other-remaining-jobs
              (cons (get-next-job scheduler inner-ready-job)
                    other-remaining-jobs)))
        (set-alt-worker-remaining-jobs! k/worker new-remaining-jobs)
        (when (not (parse-failure? result))
          (let ([result-contents (stream-first result)]
                [this-next-job (get-next-job scheduler job)])
            (define result-stream
              (parse-stream result-contents this-next-job scheduler))
            (cache-result! scheduler job result-stream)
            (set-alt-worker-job! k/worker this-next-job)
            (set-parser-job-continuation/worker! this-next-job k/worker)
            (set-parser-job-continuation/worker! job #f)))
        (run-scheduler scheduler)]
       ;; no k/worker case
       [#f (cond
             [(stream? result-stream)
              ;; IE the case of a procedural parser with a result stream to force
              (do-run! scheduler (λ () (stream-rest result-stream)) job #f #f)]
             [(equal? 0 result-index)
              ;; IE the first run of a parser
              (match parser
                [(proc-parser procedure)
                 (do-run! scheduler
                          (λ () (procedure (scheduler-input-string scheduler)
                                           pos))
                          job #f #f)]
                [(alt-parser parsers)
                 (set-parser-job-continuation/worker!
                  job
                  (alt-worker job (map (λ (p) (get-job scheduler p pos 0)) parsers)))
                 (run-scheduler scheduler)])]
             [else
              ;; In this case there has been a result stream but it is dried up.
              (cache-result! scheduler job empty-stream)
              (run-scheduler scheduler)])])]))

(define (do-run! scheduler thunk/k job continuation-run? k-arg)
  ;; When continuation-run? is true, we are running a continuation
  ;; (instead of a fresh thunk) and we want to supply k-arg.
  ;; This keeps us from growing the continuation at all when recurring.
  (define (result-loop new-thunk)
    (if new-thunk
        (call-with-continuation-prompt new-thunk chido-parse-prompt result-loop)
        recursive-enter-flag))
  (define result
    (if continuation-run?
        (call-with-continuation-prompt thunk/k chido-parse-prompt result-loop k-arg)
        (result-loop (λ ()
                       (parameterize ([current-chido-parse-job job])
                         (call-with-continuation-prompt thunk/k
                                                        parse*-direct-prompt))))))
  (let flatten-loop ([result result])
    (if (eq? result recursive-enter-flag)
        (run-scheduler scheduler)
        (if (and (stream? result)
                 (not (flattened-stream? result))
                 (not (stream-empty? result)))
            (flatten-loop
             (call-with-continuation-prompt
              (λ () (parameterize ([current-chido-parse-job job])
                      (delimit-parse*-direct (stream-flatten result))))
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
  (match result
    [(? parse-failure?) (set-parser-job-result! job result)]
    [(? stream?)
     ;; Recur with stream-first, setting the stream as the result-stream.
     ;; Note that because we have already flattened result streams, stream-first
     ;; will never itself return a stream.
     (define next-job (get-next-job scheduler job))
     (set-parser-job-result-stream! next-job result)
     (cache-result!/procedure-job scheduler job (stream-first result))]
    [(? parse-derivation?)
     (define next-job (get-next-job scheduler job))
     (define wrapped-result
       (parse-stream result next-job scheduler))
     (set-parser-job-result! job wrapped-result)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Miscellaneous

(define chido-parse-prompt (make-continuation-prompt-tag 'chido-parse-prompt))
(define parse*-direct-prompt (make-continuation-prompt-tag 'parse*-direct-prompt))
(define-syntax-rule (delimit-parse*-direct e)
  (call-with-continuation-prompt (λ () e) parse*-direct-prompt))

(define recursive-enter-flag (gensym 'recursive-enter-flag))

(define (get-next-job scheduler job)
  (match job
    [(parser-job parser start-position result-index _ _ _)
     (get-job scheduler parser start-position (add1 result-index))]))

(define (parse-stream result next-job scheduler)
  (stream-cons result (if next-job
                          (enter-the-parser scheduler next-job)
                          empty-stream)))
(define (parse-failure? x) (and (stream? x) (stream-empty? x)))

(define (derivations->end derivations)
  (apply max (map parse-derivation-end-position derivations)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Tests

(module+ test
  (require rackunit)

  (define s1 "aaaaa")

  (define (a1-parser-proc in-string position)
    (define c (and (< position (string-length in-string))
                   (string-ref in-string position)))
    (if (equal? c #\a)
        (make-parse-derivation "a" '() (add1 position))
        empty-stream))
  (define a1-parser-obj (proc-parser a1-parser-proc))

  (define (Aa-parser-proc in-string pos)
    (for/parse
     ([d/A (parse* in-string (get-A-parser) pos)])
     (for/parse
      ([d/a (parse* in-string a1-parser-obj (parse-derivation-end-position d/A))])
      (make-parse-derivation (string-append (parse-derivation-result d/A)
                                            (parse-derivation-result d/a))
                             (list d/A d/a)
                             (derivations->end (list d/A d/a))))))

  (define Aa-parser-obj (proc-parser Aa-parser-proc))


  (define A-parser (alt-parser (list Aa-parser-obj a1-parser-obj)))
  (define (get-A-parser) A-parser)

  (define results1 (parse* s1 A-parser 0))
  (check-equal? (map parse-derivation-result (stream->list results1))
                (list "a" "aa" "aaa" "aaaa" "aaaaa"))


  (define (Aa-parser-proc/direct in-string pos)
    (define d/A (parse*-direct in-string (get-A-parser/direct) pos))
    (define d/a (parse*-direct in-string a1-parser-obj
                               (parse-derivation-end-position d/A)))
    (make-parse-derivation (string-append (parse-derivation-result d/A)
                                          (parse-derivation-result d/a))
                           (list d/A d/a)
                           (derivations->end (list d/A d/a))))
  (define Aa-parser-obj/direct
    (proc-parser Aa-parser-proc/direct))
  (define A-parser/direct (alt-parser (list Aa-parser-obj/direct a1-parser-obj)))
  (define (get-A-parser/direct) A-parser/direct)

  (define results2 (parse* s1 A-parser/direct 0))
  (check-equal? (map parse-derivation-result (stream->list results2))
                (list "a" "aa" "aaa" "aaaa" "aaaaa"))
  )
