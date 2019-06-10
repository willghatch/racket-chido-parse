#lang racket/base
(require racket/contract/base)
(provide
 make-parse-derivation
 parse-derivation?
 (rename-out [parse-derivation-result! parse-derivation-result])
 parse-derivation-parser
 parse-derivation-line
 parse-derivation-column
 parse-derivation-start-position
 parse-derivation-end-position
 parse-derivation-derivation-list

 ;; this is not for the public interface...
 current-chido-parse-derivation-implicit-end

 (struct-out alt-parser)
 make-alt-parser
 ;(struct-out proc-parser)
 (rename-out [make-proc-parser proc-parser])
 proc-parser?
 ;prop:custom-parser

 parser-name
 parser-prefix
 parser-potentially-left-recursive?
 parser-potentially-null?
 parser?

 (struct-out parse-failure)
 make-parse-failure

 get-counts!

 regexp->parser

 parse*
 #;(contract-out
  [parse* (->* (input-port? parser?)
               (#:args (listof any/c)
                #:start integer?)
               (or/c parse-failure? stream?))]
  )
 ;; TODO - other parse functions

 )

(require
 "port-broker.rkt"
 "util.rkt"
 "structs.rkt"
 "stream-flatten.rkt"
 "parameters.rkt"
 racket/stream
 racket/match
 racket/struct
 racket/string
 racket/exn
 data/gvector
 (for-syntax
  racket/base
  syntax/parse
  ))

(module+ test
  (require
   rackunit
   "parse-stream.rkt"
   ))

;; TODO - explanation from notes about the big picture of how this parsing library works

;; TODO - literal strings and regexps should be accepted as parsers
;; TODO - procedures with the contract (-> parser) should be accepted as parsers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Structs

(struct parse-derivation
  ;; TODO - document from notes
  (
   [result #:mutable] [result-forced? #:mutable]
   parser
   line column start-position end-position
   derivation-list
   )
  #:transparent)

(define (parse-derivation-result! pd)
  (if (parse-derivation-result-forced? pd)
      (parse-derivation-result pd)
      (let* ([f (parse-derivation-result pd)]
             [r (with-handlers ([(λ(e)#t)(λ(e)e)])
                  (f (parse-derivation-line pd)
                     (parse-derivation-column pd)
                     (parse-derivation-start-position pd)
                     (parse-derivation-end-position pd)
                     (parse-derivation-derivation-list pd)))])
        (set-parse-derivation-result! pd r)
        (set-parse-derivation-result-forced?! pd #t)
        r)))

(define parse-enter-counter 0)
(define (inc-parse-enter!)
  (set! parse-enter-counter (add1 parse-enter-counter)))
(define find-work-counter 0)
(define (inc-find-work!)
  (set! find-work-counter (add1 find-work-counter)))
(define run-scheduler-counter 0)
(define (inc-run-scheduler!)
  (set! run-scheduler-counter (add1 run-scheduler-counter)))
(define (get-counts!)
  (eprintf "enter: ~a, run scheduler: ~a, find work: ~a\n"
           parse-enter-counter run-scheduler-counter find-work-counter)
  (set! parse-enter-counter 0)
  (set! find-work-counter 0)
  (set! run-scheduler-counter 0)
  )

(define current-chido-parse-derivation-implicit-end (make-parameter #f))

(define (make-parse-derivation result
                               #:end [end #f]
                               #:derivations [derivation-list '()])
  ;; `result` should be a non-procedure OR a procedure that accepts
  ;; line column start-position end-position derivation-list
  (define job (current-chido-parse-job))
  (match job
    [(parser-job parser extra-arguments cp-params start-position result-index
                 continuation/worker dependents result-stream)
     (define end-use (or end
                         (and (not (null? derivation-list))
                              (apply max
                                     (map parse-derivation-end-position
                                          derivation-list)))
                         (current-chido-parse-derivation-implicit-end)
                         (error 'make-parse-derivation
                                "Couldn't infer end location and none provided.")))
     (define delayed? (procedure? result))
     (parse-derivation result (not delayed?)
                       parser
                       #f #f start-position end-use
                       derivation-list)]
    [else (error 'make-parse-derivation
                 "Not called during the dynamic extent of chido-parse...")]))

(struct parser-struct (name) #:transparent)

(struct proc-parser parser-struct
  ;; TODO - document from notes
  (prefix procedure preserve-prefix? promise-no-left-recursion?)
  #:transparent)
(define (make-proc-parser
         prefix proc
         #:name [name (object-name proc)]
         #:preserve-prefix? [preserve-prefix? #f]
         #:promise-no-left-recursion? [promise-no-left-recursion? #f])
  (proc-parser name prefix proc preserve-prefix? promise-no-left-recursion?))

(struct alt-parser parser-struct
  ;; TODO - use a prefix trie for the parsers
  (parsers extra-arg-lists left-recursive? null?)
  #:transparent)

(define (make-alt-parser name parsers [extra-arg-lists #f])
  (define l-recursive? (ormap parser-potentially-left-recursive? parsers))
  (define null? (ormap parser-potentially-null? parsers))
  (alt-parser name
              parsers
              (or extra-arg-lists (map (λ (p) '()) parsers))
              l-recursive?
              null?))

(define-values (prop:custom-parser custom-parser? custom-parser-ref)
  ;; TODO - document -- the property should be a function that accepts a `self` argument and returns a parser.
  ;; TODO - Is this really a good idea?  Perhaps I really just want to have objects that users can convert into parsing procedures, eg. readtable-like-object with ->single-read, ->multi-read functions.
  (make-struct-type-property 'custom-parser))

(define (parser? p)
  (cond [(parser-struct? p)]
        [(custom-parser? p)]
        [(string? p)]
        [(regexp? p)]
        ;; TODO - this is not a great predicate...
        [(procedure? p)]
        [else #f]))

(define (parser-name p)
  (cond [(parser-struct? p) (parser-struct-name p)]
        [(custom-parser? p) (parser-name (parser->usable p))]
        [(string? p) p]
        [(regexp? p) (format "~s" p)]
        [(procedure? p) (parser-name (p))]
        [else (error 'parser-name "not a parser: ~s" p)]))

(define (parser-prefix p)
  (cond [(proc-parser? p) (proc-parser-prefix p)]
        [(string? p) p]
        [else ""]))

(define (parser-potentially-left-recursive? p)
  (cond [(alt-parser? p) (alt-parser-left-recursive? p)]
        [(proc-parser? p) (and (not (proc-parser-promise-no-left-recursion? p))
                               (or (equal? "" (proc-parser-prefix p))
                                   (proc-parser-preserve-prefix? p)))]
        [(string? p) #f]
        ;; TODO - this is not great, but I need this predicate to work while
        ;;        *constructing* parsers...
        [(procedure? p) #t]
        [else (error 'parser-potentially-left-recursive?
                     "Not yet implemented for: ~s" p)]))

(define (parser-potentially-null? p)
  (cond [(alt-parser? p) (alt-parser-null? p)]
        [(proc-parser? p) (equal? "" (proc-parser-prefix p))]
        [(equal? p "") #t]
        [(string? p) #f]
        [else (error 'parser-potentially-null?
                     "Not yet implemented for: ~s" p)]))

(define parser-cache (make-weak-hasheq))
(define (parser->usable p)
  (define (rec p)
    (cond [(parser-struct? p) p]
          [(custom-parser? p) (rec ((custom-parser-ref p) p))]
          [(string? p) p]
          ;[(string? p) (string->parser p)]
          [(regexp? p) (regexp->parser p)]
          [(procedure? p) (rec (p))]
          [else (error 'chido-parse "not a parser: ~s" p)]))
  (define cached (hash-ref parser-cache p #f))
  (or cached
      (let ([result (rec p)])
        (hash-set! parser-cache p result)
        result)))

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

(define (make-parse-failure message #:position [pos #f] #:failures [failures '()])
  (define job (current-chido-parse-job))
  (match job
    [(parser-job parser extra-arguments cp-params start-position result-index
                 continuation/worker dependents result-stream)
     (parse-failure (parser-name parser) start-position (or pos start-position)
                    message failures)]))

(define (make-cycle-failure job)
  (match job
    [(parser-job parser extra-arguments cp-params start-position result-index
                 k/worker dependents result-stream)
     (parse-failure (parser-name parser)
                    start-position
                    start-position
                    "Cycle failure"
                    '())]))

(define (exn->failure e job)
  (let ([message (format "Exception while parsing: ~a\n" (exn->string e))])
    (match job
      [(parser-job parser extra-arguments cp-params start-position
                   result-index continuation/worker dependents result-stream)
       (parse-failure (parser-name parser)
                      start-position start-position message '())])))

(define (alt-worker->failure aw)
  (match aw
    [(alt-worker job remaining-jobs ready-jobs failures successful?)
     (match job
       [(parser-job parser extra-arguments cp-params start-position result-index
                    k/worker dependents result-stream)
        (match parser
          [(alt-parser name parsers extra-arg-lists l-rec nullable)
           ;; TODO - what is the best fail position?
           ;;        Probably I should analyze the sub-failures...
           (define fail-position start-position)
           (parse-failure name start-position fail-position
                          "TODO - better failure message" failures)])])]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Literal parsers

(define (parse-regexp port r failure-message)
  (define-values (line col pos) (port-next-location port))
  (define m (regexp-match r port))
  (define length (and m (string-length (car m))))
  (if m
      (make-parse-derivation m #:end (+ pos length))
      (make-parse-failure failure-message #:position pos)))

(define (regexp->parser rx #:name [name #f])
  ;; TODO - add optional args for what to do with the result
  (define failure-message (format "Didn't match regexp: ~a" (or name rx)))
  (make-proc-parser #:name (or name (format "~a" rx))
                    ""
                    (λ (p) (parse-regexp p rx failure-message))
                    #:promise-no-left-recursion? #t))


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
  (port-broker top-job-stack done-k-stack hint-stack-stack
               job-info->job-cache job->result-cache)
  #:mutable
  #:transparent)
(define (make-scheduler port-broker)
  (scheduler port-broker '() '() '() (make-start-position-cache) (hasheq)))

(define (pop-hint! scheduler)
  (define hss (scheduler-hint-stack-stack scheduler))
  (set-scheduler-hint-stack-stack! scheduler
                                   (cons (cdr (car hss)) (cdr hss))))
(define (push-hint! scheduler hint)
  (define hss (scheduler-hint-stack-stack scheduler))
  (set-scheduler-hint-stack-stack! scheduler (cons (cons hint (car hss))
                                                   (cdr hss))))

(struct parser-job
  ;; TODO - document from notes
  (
   ;; parser is either a parser struct or an alt-parser struct
   parser
   extra-arguments
   ;; chido-parse-parameters
   cp-params
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
  (if (not p) p (parser-name p)))
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

#|
The job cache is a multi-level dictionary with the following keys / implementations:
* start-position / gvector
* parser / mutable hasheq
* result-number / gvector
* extra-args / mutable hashequal?
* cp-params / mutable hashequal?

TODO - does it make sense to use hasheq for extra-args and cp-params?
TODO - should start-position use a gvector?  Many start positions will not actually be used for parsing, probably.
TODO - perhaps alists instead of hashes for things that likely have a small number of entries?
|#
(define (make-start-position-cache)
  (make-gvector #:capacity 1000))
(define (make-parser-cache)
  (make-hasheq))
(define (make-result-number-cache)
  (make-gvector #:capacity 10))
(define (make-extra-args-cache)
  (make-hash))
(define (make-cp-params-cache)
  (make-hash))


(define (get-job! s parser extra-args cp-params start-position result-number)
  (define (make-parser-job parser)
    (parser-job parser extra-args cp-params start-position result-number #f '() #f))
  ;; traverse-cache returns the contents if `update` is false.
  (define (traverse-cache parser update)
    (define c/start-pos (scheduler-job-info->job-cache s))
    (define c/parser
      (cond [(< start-position (gvector-count c/start-pos))
             (gvector-ref c/start-pos start-position)]
            [update (for ([i (in-range (gvector-count c/start-pos)
                                       (add1 start-position))])
                      (gvector-add! c/start-pos
                                    (make-parser-cache)))
                    (gvector-ref c/start-pos start-position)]
            [else #f]))
    (define c/result-n
      (and c/parser
           (let ([r (hash-ref c/parser parser #f)])
             (cond [r r]
                   [update (let ([c (make-result-number-cache)])
                             (hash-set! c/parser parser c)
                             c)]
                   [else #f]))))
    (define c/extra-args
      (and c/result-n
           (cond [(< result-number (gvector-count c/result-n))
                  (gvector-ref c/result-n result-number)]
                 [update (for ([i (in-range (gvector-count c/result-n)
                                            (add1 result-number))])
                           (gvector-add! c/result-n (make-extra-args-cache)))
                         (gvector-ref c/result-n result-number)]
                 [else #f])))
    (define c/cp-params
      (and c/extra-args
           (let ([r (hash-ref c/extra-args extra-args #f)])
             (cond [r r]
                   [update (let ([c (make-cp-params-cache)])
                             (hash-set! c/extra-args extra-args c)
                             c)]
                   [else #f]))))
    (define value
      (and c/extra-args
           (let ([r (hash-ref c/cp-params cp-params #f)])
             (cond [r r]
                   [update
                    (hash-set! c/cp-params cp-params update)
                    update]
                   [else #f]))))
    value)

  (define usable (parser->usable parser))
  (define existing (traverse-cache usable #f))
  (or existing
      (traverse-cache usable (make-parser-job usable))))

(define (get-next-job! s job)
  (match job
    [(parser-job parser extra-args cp-params start-position
                 result-index k/w deps result-stream)
     (get-job! s parser extra-args cp-params start-position (add1 result-index))]))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Scheduling

(define chido-parse-prompt (make-continuation-prompt-tag 'chido-parse-prompt))
(define current-chido-parse-job (make-parameter #f))
(define recursive-enter-flag (gensym 'recursive-enter-flag))

(define (enter-the-parser port-broker parser extra-args start-position)
  (define s (port-broker-scheduler port-broker))
  (define cp-params (current-chido-parse-parameters))
  (define j (get-job! s parser extra-args cp-params start-position 0))
  (enter-the-parser/job s j))

(define (enter-the-parser/job scheduler job)
  (define ready-result (scheduler-get-result scheduler job))
  (or ready-result
      (let* ([old-start (and (current-chido-parse-job)
                             (parser-job-start-position (current-chido-parse-job)))]
             [new-start (parser-job-start-position job)]
             [parser (parser-job-parser job)]
             [left-recursive? (and (eq? old-start new-start)
                                   (parser-potentially-left-recursive? parser))])
        (inc-parse-enter!)
        (when (and old-start (< new-start old-start))
          (error 'chido-parse
                 "Recursive parse tried to recursively parse to the left of its starting position: ~a"
                 (parser-name (parser-job-parser (current-chido-parse-job)))))
        (if (and left-recursive? (current-chido-parse-job))
            ;; This is a (potentially left-) recursive call.
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
                 (abort-current-continuation chido-parse-prompt))
               chido-parse-prompt))
            ;; This is the original entry into the parser machinery.
            ;; Or a recursive call that isn't a left-recursion.
            (let ()
              (define result
                (let ()
                  (define k-job (scheduled-continuation #f #f job #f))
                  (push-parser-job-dependent! job k-job)
                  ;; As a fresh entry into the parser, we start a fresh hint stack.
                  (set-scheduler-hint-stack-stack!
                   scheduler (cons (list job)
                                   (scheduler-hint-stack-stack scheduler)))
                  (set-scheduler-done-k-stack!
                   scheduler (cons k-job
                                   (scheduler-done-k-stack scheduler)))
                  (set-scheduler-top-job-stack!
                   scheduler (cons job (scheduler-top-job-stack scheduler)))
                  (run-scheduler scheduler)))
              (begin
                (set-scheduler-done-k-stack!
                 scheduler (cdr (scheduler-done-k-stack scheduler)))
                (set-scheduler-top-job-stack!
                 scheduler (cdr (scheduler-top-job-stack scheduler)))
                (set-scheduler-hint-stack-stack!
                 scheduler (cdr (scheduler-hint-stack-stack scheduler)))
                result))))))


(define (find-work s job-stacks)
  ;; Returns a hint stack for an actionable job -- IE a list where the
  ;; first element is an actionable job, the second element is a job
  ;; that depends on the first, etc
  ;; OR
  ;; Returns #f if there is no actionable job.
  ;;
  ;; s is a scheduler
  ;; job-stacks is a list of these dependency stacks
  ;; TODO - this is probably the best place to detect cycles.  I should maybe return some sort of flag object containing the job that contains the smallest dependency cycle so I know which job to return a cycle error for.
  (inc-find-work!)
  (let loop ([stacks job-stacks]
             [blocked '()])
    (if (null? stacks)
        #f
        (let* ([jstack (car stacks)]
               [j (car jstack)])
          (cond [(memq j blocked) (loop (cdr stacks) blocked)]
                ;; if it has a continuation/worker, add dependencies to jobs
                [else (match (parser-job-continuation/worker j)
                        [(scheduled-continuation job k dependency ready?)
                         (cond [ready? jstack]
                               [else (loop (cons (cons dependency jstack)
                                                 (cdr stacks))
                                           (cons j blocked))])]
                        [(alt-worker job remaining-jobs ready-jobs
                                     failures successful?)
                         (cond [(not (null? ready-jobs)) jstack]
                               [(null? remaining-jobs) jstack]
                               [else (loop (append (map (λ (rj) (cons rj jstack))
                                                        remaining-jobs)
                                                   (cdr stacks))
                                           (cons j blocked))])]
                        [#f jstack])])))))

(define (find-and-run-actionable-job s hint-stacks using-hint?)
  (define (run-actionable-job? j)
    ;; TODO - this is probably not great, but for now I just want to get things working again...
    (if (scheduler-get-result s j)
        (run-scheduler s)
        (run-actionable-job s j)))
  (let* ([actionable-job-stack (find-work s hint-stacks)]
         [actionable-job (and actionable-job-stack (car actionable-job-stack))])
    (and actionable-job-stack (set-scheduler-hint-stack-stack!
                               s (cons actionable-job-stack
                                       (cdr (scheduler-hint-stack-stack s)))))
    (cond
      [(and actionable-job (scheduler-get-result s actionable-job))
       (pop-hint! s)
       (ready-dependents! actionable-job)
       (run-scheduler s)]
      [actionable-job (run-actionable-job? actionable-job)]
      [using-hint?
       ;; try to find work in another part of the work tree...
       (set-scheduler-hint-stack-stack!
        s (cons '() (cdr (scheduler-hint-stack-stack s))))
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
     (scheduler-get-result s (scheduled-continuation-dependency done-k))]
    [else
     (inc-run-scheduler!)
     (define using-hint? #t)
     (when (null? (car (scheduler-hint-stack-stack s)))
       (set! using-hint? #f)
       (push-hint! s (car (scheduler-top-job-stack s))))
     (define hint-stack (car (scheduler-hint-stack-stack s)))
     (define hint-worker (parser-job-continuation/worker (car hint-stack)))
     (define (run-actionable-job? j)
       ;; TODO - this is probably not great, but for now I just want to get things working again...
       (if (scheduler-get-result s j)
           (run-scheduler s)
           (run-actionable-job s j)))
     (match hint-worker
       [#f (if (scheduler-get-result s (car hint-stack))
               (begin (pop-hint! s)
                      (run-scheduler s))
               (run-actionable-job? (car hint-stack)))]
       [(scheduled-continuation job k dependency (and #t ready?))
        (pop-hint! s)
        (run-actionable-job? job)]
       [(scheduled-continuation job k dependency ready?)
        (find-and-run-actionable-job s (list hint-stack) using-hint?)]
       [(alt-worker job remaining-jobs (list ready-job rjs ...) failures successful?)
        (pop-hint! s)
        (run-actionable-job? job)]
       [(alt-worker job (list) (list) failures successful?)
        (pop-hint! s)
        (run-actionable-job? job)]
       [(alt-worker job remaining-jobs ready-jobs failures successful?)
        ;; No ready jobs
        (find-and-run-actionable-job s (list hint-stack) using-hint?)]
       )]))


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
       (if (memq dependency jobs)
           (begin
             (set-scheduled-continuation-dependency!
              goal
              (cycle-breaker-job dependency))
             (set-scheduled-continuation-ready?! goal #t))
           (rec (parser-job-continuation/worker dependency)
                (cons job jobs)))]))
  (rec (car (scheduler-done-k-stack scheduler)) '()))

(define (run-actionable-job scheduler job)
  (when (scheduler-get-result scheduler job)
    (error 'chido-parse
           "internal error - run-actionable-job got a job that was already done: ~a"
           (job->display job)))
  (match job
    [(parser-job parsador extra-args cp-params start-position
                 result-index k/worker dependents result-stream)
     (match k/worker
       [(scheduled-continuation job k dependency (and ready? #t))
        (do-run! scheduler
                 k
                 job
                 #t
                 (scheduler-get-result scheduler dependency))]
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
                        job
                        #f #f)]
              [(equal? 0 result-index)
               (match parsador
                 [(proc-parser name prefix procedure pp no-lr)
                  (define port (port-broker->port (scheduler-port-broker
                                                   scheduler)
                                                  start-position))
                  (define prefix-length (string-length prefix))
                  ;; TODO - optimize this peek for alt parsers, at least...
                  (if (equal? prefix
                              (peek-string prefix-length 0 port))
                      (do-run! scheduler
                               (λ ()
                                 (parameterize ([current-chido-parse-parameters
                                                 cp-params])
                                   (let ([result (apply procedure port extra-args)])
                                     (if (and (stream? result)
                                              (not (flattened-stream? result)))
                                         (stream-flatten result)
                                         result))))
                               job
                               #f #f)
                      (let ([result (parse-failure name start-position start-position
                                                   "prefix didn't match" '())])
                        (cache-result-and-ready-dependents! scheduler job result)
                        (run-scheduler scheduler)))]
                 [(alt-parser name parsers extra-arg-lists l-rec nullable)
                  (define (mk-dep parsador extra-args)
                    (get-job! scheduler parsador extra-args cp-params start-position 0))
                  (define worker
                    (alt-worker job (map mk-dep parsers extra-arg-lists) '() '() #f))
                  (for ([dep (alt-worker-remaining-jobs worker)])
                    (push-parser-job-dependent! dep worker))
                  (set-parser-job-continuation/worker! job worker)
                  ;(push-hint! scheduler worker)
                  (run-scheduler scheduler)]
                 [(? string?)
                  (define s parsador)
                  (define pb (scheduler-port-broker scheduler))
                  (define length (string-length s))
                  (define match?
                    (for/and ([string-index (in-range length)]
                              [input-index (in-range start-position
                                                     (+ start-position length))])
                      (eq? (string-ref s string-index)
                           (port-broker-char pb input-index))))
                  (define result
                    (parameterize ([current-chido-parse-job job])
                      (if match?
                          (parse-stream
                           (make-parse-derivation s #:end (+ start-position length))
                           #f
                           scheduler)
                          (make-parse-failure (format "literal didn't match: ~s" s)
                                              #:position start-position))))
                  (cache-result-and-ready-dependents! scheduler job result)
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

(define (do-run! scheduler thunk job continuation-run? k-arg)
  (when (not job)
    (error 'chido-parse "Internal error - trying to recur with no job"))
  #|
  When continuation-run? is true, we are running a continuation (instead of a fresh thunk) and we want to supply k-arg.
  This keeps us from growing the continuation at all when recurring.
  |#
  (define result
    (if continuation-run?
        (call-with-continuation-prompt
         thunk
         chido-parse-prompt
         (λ () recursive-enter-flag)
         k-arg)
        (call-with-continuation-prompt
         (λ ()
           (with-handlers ([(λ (e) #t) (λ (e) (exn->failure e job))])
             (parameterize ([current-chido-parse-job job])
               (thunk))))
         chido-parse-prompt
         (λ () recursive-enter-flag))))
  (if (eq? result recursive-enter-flag)
      (run-scheduler scheduler)
      (begin
        (cache-result-and-ready-dependents! scheduler job result)
        (run-scheduler scheduler))))

(define (ready-dependents! job)
  (for ([dep (parser-job-dependents job)])
    (match dep
      [(alt-worker alt-job remaining-jobs ready-jobs failures successful?)
       (set-alt-worker-ready-jobs! dep (cons job ready-jobs))]
      [(scheduled-continuation parent-job k dependency ready?)
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
       [(parser-job parse extra-arguments cp-params start-position
                    result-index continuation/worker
                    dependents result-stream)
        (match parse
          [(proc-parser name prefix procedure pp no-lr)
           (define failure
             (let ([inner-failures (and (flattened-stream? result)
                                        (flattened-stream-failures result))])
               (match inner-failures
                 [(or #f (list))
                  (parse-failure name start-position start-position
                                 "parse returned empty stream" '())]
                 [(list one-fail) one-fail]
                 [(list fail ...)
                  (define best-fail (car (sort fail <
                                               #:key parse-failure-fail-position
                                               #:cache-keys? #t)))
                  (parse-failure name
                                 start-position
                                 (parse-failure-fail-position best-fail)
                                 (format "Multiple failures: best choice: ~a"
                                         (parse-failure-message best-fail))
                                 fail)])))
           (cache-result-and-ready-dependents!/procedure-job
            scheduler
            job
            failure)])])]
    [(? stream?)
     ;; Recur with stream-first, setting the stream as the result-stream
     (define next-job (get-next-job! scheduler job))
     (set-parser-job-result-stream! next-job result)
     (cache-result-and-ready-dependents!/procedure-job
      scheduler job (raw-result->parse-derivation job (stream-first result)))]
    [(parse-derivation semantic-result result-forced?
                       parser
                       line column start-position end-position
                       derivation-list)
     (define next-job (get-next-job! scheduler job))
     (define wrapped-result
       (parse-stream result next-job scheduler))
     (scheduler-set-result! scheduler job wrapped-result)
     (ready-dependents! job)]
    [else (cache-result-and-ready-dependents! scheduler
                                              job
                                              (raw-result->parse-derivation
                                               job
                                               result))]))

(define (raw-result->parse-derivation job result)
  (if (parse-derivation? result)
      result
      (error 'TODO "auto-transformation to proper parse result not yet implemented.  In job: ~a, got ~a\n" (job->display job) result)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Parsing outer API

(define (parse* port parser
                #:args [extra-args '()]
                #:start [start #f])
  (define pb (or (port->port-broker port)
                 (port-broker port)))
  (define start-pos (match start
                      [(? number?) start]
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
  (define a1-parser-obj (make-proc-parser #:name "a" "" a1-parser-proc))

  (define (Aa-parser-proc port)
    (for/parse
     ([d/A (parse* port (get-A-parser))])
     (for/parse
      ([d/a (parse* port a1-parser-obj
                    #:start d/A)])
      (make-parse-derivation (λ args (string-append (parse-derivation-result! d/A)
                                                    (parse-derivation-result! d/a)))
                             #:derivations (list d/A d/a)))))

  (define Aa-parser-obj (make-proc-parser #:name "Aa" "" Aa-parser-proc))


  (define A-parser (make-alt-parser "A"
                                    (list
                                     Aa-parser-obj
                                     a1-parser-obj)))
  (define (get-A-parser) A-parser)

  (define results1 (parse* p1 A-parser))
  (check-equal? (map parse-derivation-result! (stream->list results1))
                (list "a" "aa" "aaa" "aaaa" "aaaaa"))


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
