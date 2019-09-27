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

 parse-derivation-parser?
 parse-derivation-parser-name?
 parse-derivation-left-most-subderivation
 parse-derivation-right-most-subderivation

 ;; this is not for the public interface...
 current-chido-parse-derivation-implicit-end

 alt-parser?
 make-alt-parser
 alt-parser-parsers
 alt-parser-left-recursive?
 ;alt-parser-null?
 (rename-out [make-proc-parser proc-parser])
 proc-parser?
 ;prop:custom-parser

 non-cached-parser-thunk

 parser-name
 parser-prefix
 parser-potentially-left-recursive?
 parser-potentially-null?
 parser?

 (struct-out parse-failure)
 make-parse-failure

 get-counts!

 parse*
 parse/direct-recursive
 #;(contract-out
  [parse* (->* (input-port? parser?)
               (#:args (listof any/c)
                #:start integer?)
               (or/c parse-failure? stream?))]
  )
 ;; TODO - other parse functions

 ;; From port-broker
 (rename-out
  [port-broker->port/wrap port-broker->port]
  [port-broker-char/wrap port-broker-char]
  [port-broker-line/wrap port-broker-line]
  [port-broker-column/wrap port-broker-column]
  [port-broker-substring?/wrap port-broker-substring?]
  [port-broker-substring/wrap port-broker-substring]
  [port-broker-wrap-position port-broker-start-position]
  )

 )

(require
 "port-broker.rkt"
 "util.rkt"
 "structs.rkt"
 "stream-flatten.rkt"
 "parameters.rkt"
 "trie.rkt"
 "parse-stream.rkt"
 racket/stream
 racket/match
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

;; TODO - explanation from notes about the big picture of how this parsing library works

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

(define (parse-derivation-parser? derivation parser)
  (equal? (parse-derivation-parser derivation) parser))
(define (parse-derivation-parser-name? derivation name)
  (equal? (parser-name (parse-derivation-parser derivation))
          name))

(define (parse-derivation-left-most-subderivation derivation)
  (car (parse-derivation-derivation-list derivation)))
(define (parse-derivation-right-most-subderivation derivation)
  (car (reverse (parse-derivation-derivation-list derivation))))


(define-syntax (define-counters stx)
  (syntax-parse stx
    [(_ get-counts-name! [name ...])
     (with-syntax ([(inc! ...) (map (λ (x) (format-id x "inc-~a!" x))
                                    (syntax->list #'(name ...)))]
                   [(counter ...) (generate-temporaries #'(name ...))])
       #'(begin
           (define counter 0) ...
           (define (inc!) (set! counter (add1 counter))) ...
           (define (get-counts-name!)
             (eprintf "~a: ~a\n" 'name counter) ...
             (eprintf "\n")
             (set! counter 0) ...)))]))
(define-counters get-counts! (parse-enter
                              find-work
                              find-work-loop
                              actionable-job-false
                              potential-left-recursion
                              run-scheduler
                              no-hint
                              traverse-cache
                              ))

(define current-chido-parse-derivation-implicit-end (make-parameter #f))

(define (make-parse-derivation result
                               #:end [end #f]
                               #:derivations [derivation-list '()])
  ;; `result` should be a non-procedure OR a procedure that accepts
  ;; line column start-position end-position derivation-list
  (define job (current-chido-parse-job))
  (match job
    [(s/kw parser-job
           #:parser parser
           #:scheduler scheduler
           #:start-position start-position)
     (define pb (scheduler-port-broker scheduler))
     (define line (port-broker-line pb start-position))
     (define column (port-broker-column pb start-position))
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
                       line column start-position end-use
                       derivation-list)]
    [else (error 'make-parse-derivation
                 "Not called during the dynamic extent of chido-parse...")]))

(struct parser-struct (name) #:transparent)

(struct proc-parser parser-struct
  ;; TODO - document from notes
  (prefix procedure preserve-prefix? promise-no-left-recursion? use-port?)
  #:transparent)
(define (make-proc-parser
         prefix proc
         #:name [name (format "~a" (object-name proc))]
         #:preserve-prefix? [preserve-prefix? #f]
         #:promise-no-left-recursion? [promise-no-left-recursion? #f]
         #:use-port? [use-port? #t])
  (proc-parser name prefix proc
               preserve-prefix? promise-no-left-recursion? use-port?))

(struct alt-parser parser-struct
  (parsers left-recursive? null? trie)
  #:transparent)

(define (make-alt-parser name parsers)
  (define l-recursive? (ormap parser-potentially-left-recursive? parsers))
  (define null? (ormap parser-potentially-null? parsers))
  (define trie (for/fold ([t empty-trie])
                         ([p parsers])
                 (trie-add t (parser-prefix p) p)))
  (alt-parser name
              parsers
              l-recursive?
              null?
              trie))

(define-values (prop:custom-parser custom-parser? custom-parser-ref)
  ;; TODO - document -- the property should be a function that accepts a `self` argument and returns a parser.
  ;; TODO - Is this really a good idea?  Perhaps I really just want to have objects that users can convert into parsing procedures, eg. readtable-like-object with ->single-read, ->multi-read functions.
  (make-struct-type-property 'custom-parser))

(define (parser? p)
  (cond [(parser-struct? p)]
        [(custom-parser? p)]
        [(string? p)]
        ;; TODO - this is not a great predicate...
        [(procedure? p)]
        [else #f]))

(define (parser-name p)
  (cond [(parser-struct? p) (parser-struct-name p)]
        [(custom-parser? p) (parser-name (parser->usable p))]
        [(string? p) p]
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

(struct non-cached-parser-thunk (proc)
  #:property prop:procedure (struct-field-index proc))

(define parser-cache (make-weak-hasheq))
(define (parser->usable p)
  (cond [(parser-struct? p) p]
        [(custom-parser? p)
         (parser->usable ((custom-parser-ref p) p))]
        [(string? p) p]
        ;; Usually I want to cache thunk results, but some, including chido-parse-parameters, specifically need NOT to be cached (at least not merely on procedure object identity), because they depend on chido-parse-parameterization.
        [(non-cached-parser-thunk? p) (parser->usable (p))]
        [(chido-parse-parameter? p) (parser->usable (p))]
        [(procedure? p)
         (define cached (hash-ref parser-cache p #f))
         (or cached
             (let ([result (parser->usable (p))])
               (hash-set! parser-cache p result)
               result))]
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

(define (make-parse-failure message #:position [pos #f] #:failures [failures '()])
  (define job (current-chido-parse-job))
  (match job
    [(s/kw parser-job #:parser parser #:start-position start-position)
     (parse-failure (parser-name parser) start-position (or pos start-position)
                    message failures)]))

(define (make-cycle-failure job job-cycle-list)
  (match job
    [(s/kw parser-job #:parser parser #:start-position start-position)
     (parse-failure (parser-name parser)
                    start-position
                    start-position
                    (format "Cycle failure: ~a" (map job->display
                                                     (reverse job-cycle-list)))
                    '())]))

(define (exn->failure e job)
  (let ([message (format "Exception while parsing: ~a\n" (exn->string e))])
    (match job
      [(s/kw parser-job #:parser parser #:start-position start-position)
       (parse-failure (parser-name parser)
                      start-position start-position message '())])))

(define (alt-worker->failure aw)
  (match aw
    [(s/kw alt-worker #:job job #:failures failures)
     (match job
       [(s/kw parser-job #:parser parser #:start-position start-position)
        (match parser
          [(s/kw alt-parser #:name name)
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
  (port-broker top-job-stack done-k-stack hint-stack-stack job-info->job-cache)
  #:mutable
  #:transparent)
(define (make-scheduler port-broker)
  (scheduler port-broker '() '() '() (make-start-position-cache)))

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
   scheduler
   ;; chido-parse-parameters
   cp-params
   start-position
   result-index
   ;; gvector of siblings indexed by result-index
   siblings
   [continuation/worker #:mutable]
   ;; dependents are scheduled-continuations or alt-workers
   [dependents #:mutable]
   ;; The actual result.
   [result #:mutable]
   ;; This one is not used for alt-parsers, but is used to keep track of the
   ;; stream from a procedure.
   [result-stream #:mutable]
   )
  #:transparent)

(define (push-parser-job-dependent! job new-dependent)
  (set-parser-job-dependents!
   job
   (cons new-dependent (parser-job-dependents job))))

(struct cycle-breaker-job (failure-job cycle-jobs) #:transparent)

(define (job->result job)
  (match job
    [(cycle-breaker-job failure-job cycle-jobs)
     (make-cycle-failure failure-job cycle-jobs)]
    [(s/kw parser-job #:result r) r]))

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

;; Here are some alist wrappers that I tried.  They don't seem to be faster
;; than using hashes anywhere I've tried, but I guess I don't want to delete
;; them just yet.
;(struct malist ([content #:mutable]))
;(define (malist-ref malist key)
;  (let ([pair (assq key (malist-content malist))])
;    (and pair (cdr pair))))
;(define (malist-set! malist key val)
;  (set-malist-content! malist
;                       (cons (cons key val)
;                             (malist-content malist))))
;(define (make-malist) (malist '()))

#|
The job cache is a multi-level dictionary with the following keys / implementations:
* start-position / gvector
* parser / mutable hasheq
* cp-params / mutable hashequal?
|#
(define (make-start-position-cache)
  (make-gvector #:capacity 1000))
(define (make-parser-cache)
  (make-hasheq))
(define (make-cp-params-cache)
  (make-hash))


(define (get-job-0! s parser cp-params start-position)
  (define (make-fresh-parser-job usable)
    (define siblings-vec (make-gvector))
    (define job
      (parser-job usable s cp-params start-position
                  0 siblings-vec #f '() #f #f))
    (gvector-add! siblings-vec job)
    job)
  ;; traverse-cache returns the contents if `update` is false.
  (define (traverse-cache parser)
    (define update #t)
    (inc-traverse-cache!)
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
    ;; This seems to make a difference for some benchmarks, but it's not huge.
    #;(define value/skip-cp-params
      (and c/parser
           (let ([r (hash-ref c/parser parser #f)])
             (cond [r r]
                   [update (let ([c (make-fresh-parser-job usable)])
                             (hash-set! c/parser parser c)
                             c)]
                   [else #f]))))
    (define c/cp-params
      (and c/parser
           (let ([r (hash-ref c/parser parser #f)])
             (cond [r r]
                   [update (let ([c (make-cp-params-cache)])
                             (hash-set! c/parser parser c)
                             c)]
                   [else #f]))))
    (define value
      (and c/cp-params
           (let ([r (hash-ref c/cp-params cp-params #f)])
             (cond [r r]
                   [update
                    (define new (make-fresh-parser-job usable))
                    (hash-set! c/cp-params cp-params new)
                    new]
                   [else #f]))))
    value
    ;value/skip-cp-params
    )

  (define usable (parser->usable parser))
  (traverse-cache usable))

(define (get-next-job! job)
  (match job
    [(s/kw parser-job #:parser parser #:scheduler scheduler #:cp-params cp-params
           #:start-position start-position #:result-index result-index
           #:siblings siblings)
     (define next-index (add1 result-index))
     (if (< next-index (gvector-count siblings))
         (gvector-ref siblings next-index)
         (let ([new-job (parser-job parser scheduler cp-params start-position
                                    next-index siblings #f '() #f #f)])
           (gvector-add! siblings new-job)
           new-job))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; port-broker wraps

#|
I want to avoid using the port interface where possible, since it imposes a constant back-and-forth between bytes and strings.
So I want to use port-brokers directly to use *fewer* custom port calls.
But I still need to encapsulate the port and give a start position.
|#
(struct port-broker-wrap (broker position) #:transparent)

(define (range-err pb/w pos name message)
  (when (< pos
           (port-broker-wrap-position pb/w))
    (error name message)))

(define (port-broker->port/wrap pb/w pos)
  (range-err pb/w pos 'port-broker->port "Can't create a port with a start position lower that the current parse start position.")
  (port-broker->port (port-broker-wrap-broker pb/w) pos))
(define (port-broker-char/wrap pb/w pos)
  (range-err pb/w pos 'port-broker-char "Can't read a position lower that the current parse start position.")
  (port-broker-char (port-broker-wrap-broker pb/w) pos))
(define (port-broker-line/wrap pb/w pos)
  (range-err pb/w pos 'port-broker-line "Can't read a position lower that the current parse start position.")
  (port-broker-line (port-broker-wrap-broker pb/w) pos))
(define (port-broker-column/wrap pb/w pos)
  (range-err pb/w pos 'port-broker-column "Can't read a position lower that the current parse start position.")
  (port-broker-column (port-broker-wrap-broker pb/w) pos))
(define (port-broker-substring?/wrap pb/w pos str)
  (range-err pb/w pos 'port-broker-substring? "Can't read a position lower that the current parse start position.")
  (port-broker-substring? (port-broker-wrap-broker pb/w) pos str))
(define (port-broker-substring/wrap pb/w pos len)
  (range-err pb/w pos 'port-broker-substring "Can't read a position lower that the current parse start position.")
  (port-broker-substring (port-broker-wrap-broker pb/w) pos len))


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
              (inc-potential-left-recursion!)
              (call-with-composable-continuation
               (λ (k)
                 (define sched-k (job->scheduled-continuation parent-job k job))
                 (set-parser-job-continuation/worker! parent-job sched-k)
                 (push-parser-job-dependent! job sched-k)
                 ;(push-hint! scheduler sched-k)
                 (abort-current-continuation chido-parse-prompt #f))
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


(define (get-last-alt-stack job-stack)
  (cond [(null? job-stack) #f]
        [(alt-parser? (parser-job-parser (car job-stack))) job-stack]
        [else (get-last-alt-stack (cdr job-stack))]))

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
     (cache-result-and-ready-dependents!
      scheduler
      job
      (parse-failure (parser-name p) pos pos "prefix didn't match" '()))]
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
             (make-parse-failure (format "literal didn't match: ~s" s)
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
        (set-alt-worker-remaining-jobs! k/worker (remove ready-job remaining-jobs))
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
                  ;; TODO - optimize this peek for alt parsers, at least...
                  (if (port-broker-substring? port-broker start-position prefix)
                      (do-run! scheduler
                               (λ ()
                                 (parameterize ([current-chido-parse-parameters
                                                 cp-params])
                                   (let ([result (procedure proc-input)])
                                     result)))
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
                  (for ([dep (alt-worker-remaining-jobs worker)])
                    (push-parser-job-dependent! dep worker))
                  (set-parser-job-continuation/worker! job worker)
                  ;(push-hint! scheduler worker)
                  (run-scheduler scheduler)]
                 [(? string?)
                  (define s parser)
                  (define pb (scheduler-port-broker scheduler))
                  (define match?
                    (port-broker-substring? pb start-position s))
                  (string-job-finalize! scheduler job match?)
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
                       (with-handlers ([(λ (e) #t) (λ (e) (exn->failure e job))])
                         (parameterize ([current-chido-parse-job job])
                           (thunk/k)))))))
  (let flatten-loop ([result result])
    (if (eq? result recursive-enter-flag)
        (run-scheduler scheduler)
        (if (and (stream? result)
                 (not (flattened-stream? result)))
            (flatten-loop
             (call-with-continuation-prompt
              (λ () (parameterize ([current-chido-parse-job job])
                      (stream-flatten result)))
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
     (define next-job (get-next-job! job))
     (set-parser-job-result-stream! next-job result)
     (cache-result-and-ready-dependents!/procedure-job
      scheduler job (raw-result->parse-derivation job (stream-first result)))]
    [(s/kw parse-derivation)
     (define next-job (get-next-job! job))
     (define wrapped-result
       (parse-stream result next-job scheduler))
     (set-parser-job-result! job wrapped-result)
     (ready-dependents! job)]
    [else (cache-result-and-ready-dependents! scheduler
                                              job
                                              (raw-result->parse-derivation
                                               job
                                               result))]))

(define (raw-result->parse-derivation job result)
  (if (parse-derivation? result)
      result
      (error 'TODO "auto-transformation to proper parse result not yet implemented.  In job: ~a, got ~s\n" (job->display job) result)))



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
  (parse-inner enter-the-parser port/pbw parser start))

(define (parse/direct-recursive port parser
                                #:start [start #f])
  ;; This one lets the user get a single result back, but the return is actually a stream.
  (define (direct-recursive-parse-core port/pbw parser start)
    (call-with-composable-continuation
     (λ (k)
       (abort-current-continuation
        chido-parse-prompt
        ;; TODO - better failure handling and propagation
        (λ () (for/parse ([d (parse* port parser #:start start)])

                         ;; Get the port position right for the continuation.
                         (define new-pos (parse-derivation-end-position d))
                         (port-broker-port-reset-position port new-pos)
                         (k d)))))
     chido-parse-prompt))
  (parse-inner direct-recursive-parse-core port parser start))

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


  (define (Aa-parser-proc/direct port)
    (define d/A (parse/direct-recursive port (get-A-parser/direct)))
    (define d/a (parse/direct-recursive port a1-parser-obj #:start d/A))
    (make-parse-derivation (λ args (string-append (parse-derivation-result! d/A)
                                                  (parse-derivation-result! d/a)))
                           #:derivations (list d/A d/a)))
  (define Aa-parser-obj/direct
    (make-proc-parser #:name "Aa" "" Aa-parser-proc/direct))
  (define A-parser/direct (make-alt-parser "A"
                                           (list
                                            Aa-parser-obj/direct
                                            a1-parser-obj)))
  (define (get-A-parser/direct) A-parser/direct)

  (define results2 (parse* p1 A-parser/direct))
  (check-equal? (map parse-derivation-result! (stream->list results2))
                (list "a" "aa" "aaa" "aaaa" "aaaaa"))

  )

