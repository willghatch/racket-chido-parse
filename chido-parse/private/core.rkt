#lang racket/base
(require racket/contract/base)
(provide
 make-parse-derivation
 parse-derivation?
 (rename-out [parse-derivation-result! parse-derivation-result])
 #;(contract-out
  (rename parse-derivation-result! parse-derivation-result
          (-> parse-derivation? any/c)))
 parse-derivation-parser
 parse-derivation-source-name
 parse-derivation-line
 parse-derivation-column
 parse-derivation-start-position
 parse-derivation-end-position
 ;; TODO - replace uses of derivation-list and then rename in the struct itself.
 parse-derivation-derivation-list
 (rename-out [parse-derivation-derivation-list parse-derivation-subderivations])

 parse-derivation-parser?
 parse-derivation-parser-name?
 parse-derivation-left-most-subderivation
 parse-derivation-right-most-subderivation

 ;; this is not for the public interface...
 current-chido-parse-derivation-implicit-end
 port->pos

 alt-parser?
 ;; TODO - use the renamed `alt-parser` everywhere.  Its interface is better than `make-alt-parser`.
 make-alt-parser
 (rename-out [make-alt-parser* alt-parser])
 alt-parser-parsers
 alt-parser-left-recursive?
 ;alt-parser-null?
 (rename-out [make-proc-parser proc-parser])
 proc-parser?
 prop:custom-parser

 non-cached-parser-thunk

 parser-name
 parser-prefix
 parser-potentially-left-recursive?
 parser-potentially-null?
 parser?

 (struct-out parse-failure)
 make-parse-failure
 (rename-out [exn->failure/export exn->failure])
 parse-failure->string/location-triple
 parse-failure->string/simple
 parse-failure->string/message
 parse-failure->string/chain
 parse-failure->string/tree
 greatest-failure
 chido-parse-keep-multiple-failures?

 get-counts!

 parse*
 parse*-direct
 delimit-parse*-direct
 for/parse
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
 "ephemeron-cache.rkt"
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
  ;; • result is the parse result when result-forced? is true, otherwise it is a procedure that produces the result (-> source line column start-pos span derivation-list result).
  ;; • derivation-list is a list of derivations for sub-components.
  (
   [result #:mutable] [result-forced? #:mutable]
   parser
   source-name
   line column start-position end-position
   derivation-list
   )
  #:transparent)

(define (parse-derivation-result! pd)
  (if (parse-derivation-result-forced? pd)
      (parse-derivation-result pd)
      (let* ([f (parse-derivation-result pd)]
             [r (with-handlers ([(λ(e)#t)(λ(e)e)])
                  (f (parse-derivation-source-name pd)
                     (parse-derivation-line pd)
                     (parse-derivation-column pd)
                     (parse-derivation-start-position pd)
                     (- (parse-derivation-end-position pd)
                        (parse-derivation-start-position pd))
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


;; This counter stuff is just for help in figuring out performance information.
;; IE debug info.
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
                              cache-parser-conflict
                              cache-cp-param-conflict
                              ))

(define current-chido-parse-derivation-implicit-end (make-parameter #f))

(define (make-parse-derivation result
                               #:end [end #f]
                               #:derivations [derivations '()])
  ;; `result` should be a non-procedure OR a procedure that accepts
  ;; src line column start-position span derivation-list
  (define job (current-chido-parse-job))
  (define derivation-list (if (list? derivations) derivations (list derivations)))
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
                       (not delayed?)
                       parser
                       source-name
                       line column start-position end-use
                       derivation-list)]
    [else (error 'make-parse-derivation
                 "Not called during the dynamic extent of chido-parse.\nGiven result: ~v\n" result)]))

(struct parser-struct (name) #:transparent)

(struct proc-parser parser-struct
  ;; • prefix is a string that is the static prefix of the parser.
  ;; • procedure is the parsing procedure (-> port parse-derivation)
  ;; • preserve-prefix? is a flag that determines whether the system consumes the prefix before applying the procedure.  Normally you don't want to look at the prefix in the procedure (it's static, after all), but at times it's useful, eg. for wrapping existing functions.
  ;; • promise-no-left-recursion? is an optimization flag -- it can avoid overhead with delimited continuations.  It's really only faster in Racket-BC, though, not Racket-CS.  It's potentially unsafe in that it can cause infinite recursion when used incorrectly.
  ;; • use-port? is a flag for whether the procedure accepts a port or a port-broker-wrapper.  I think it's not really exposed publicly.  TODO - what do I want to do about it?
  (prefix procedure preserve-prefix? promise-no-left-recursion? use-port?)
  #:transparent)
(define (make-proc-parser
         proc
         #:prefix [prefix ""]
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
  (define name-use (or name "alt-parser"))
  (alt-parser name-use
              parsers
              l-recursive?
              null?
              trie))
(define (make-alt-parser* #:name [name #f] . parsers)
  (make-alt-parser name parsers))

(define-values (prop:custom-parser custom-parser? custom-parser-ref)
  ;; TODO - document -- the property should be a function that accepts a `self` argument and returns a parser.
  ;; TODO - Is this really a good idea?  Perhaps I really just want to have objects that users can convert into parsing procedures, eg. readtable-like-object with ->single-read, ->multi-read functions.
  ;; TODO - custom parsers should be able to give certain info without evaluating the function that returns a desugared parser, such as name info (eg. to break naming cycles for automatic naming), whether the parser is potentially left-recursive, etc.  Maybe the property should provide a hash table of (maybe optional) custom parser methods.
  (make-struct-type-property 'custom-parser))

(define (parser? p)
  (cond [(parser-struct? p)]
        [(custom-parser? p)]
        [(string? p)]
        ;; TODO - this is not a great predicate...
        ;; The contract really should be (-> parser?)
        [(procedure? p)]
        [else #f]))

(define (parser-name p)
  (cond [(parser-struct? p) (parser-struct-name p)]
        ;; TODO - custom parsers should be able to inform this
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
        ;; TODO - custom parsers should be able to inform this
        [(custom-parser? p) #t]
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
        ;; TODO - custom parsers should be able to inform this
        [(custom-parser? p) #t]
        ;; TODO - this is not great, but I need this predicate to work while
        ;;        *constructing* parsers...
        [(procedure? p) #t]
        [else (error 'parser-potentially-null?
                     "Not yet implemented for: ~s" p)]))

(struct non-cached-parser-thunk (proc)
  #:property prop:procedure (struct-field-index proc))

(define parser-cache (make-weak-hasheq))
(define (parser->usable p)
  (cond [(parser-struct? p) p]
        [(custom-parser? p)
         (parser->usable ((custom-parser-ref p) p))]
        [(and (string? p) (immutable? p)) p]
        [(string? p) (string->immutable-string p)]
        ;; Usually I want to cache thunk results, but some, including chido-parse-parameters, specifically need NOT to be cached (at least not merely on procedure object identity), because they depend on chido-parse-parameterization.
        [(non-cached-parser-thunk? p) (parser->usable (p))]
        [(chido-parse-parameter? p) (parser->usable (p))]
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

(define progress-default (gensym))
;; TODO - Maybe I should have a `keep-all?` parameter and a `keep-ties?` parameter to keep multiple failures when they tie for greatest.
(define chido-parse-keep-multiple-failures? (make-parameter #f))
(define (make-parse-failure #:message [message #f]
                            #:position [position #f]
                            #:end [end #f]
                            #:made-progress? [made-progress?/given progress-default]
                            #:inner-failure [inner-failure #f]
                            #:failures [failures #f])
  (define job (current-chido-parse-job))
  (match job
    [#f (error 'make-parse-failure
               "Called outside of the dynamic extent of a chido-parse parser.")]
    [(s/kw parser-job #:parser parser #:start-position start-position #:port port)
     (define all-failures (and failures
                               (remove-duplicates
                                (if inner-failure
                                    (cons inner-failure failures)
                                    failures)
                                eq?)))
     (define best-failure (or inner-failure
                              (and all-failures
                                   (not (null? all-failures))
                                   (greatest-failure all-failures))))
     (define guess-end (if port
                           (port->pos port)
                           start-position))
     (define inner-end (if all-failures
                           (and (not (null? all-failures))
                                (apply max (map parse-failure-effective-end
                                                all-failures)))
                           (and best-failure
                                (parse-failure-effective-end best-failure))))
     (define effective-end (max (or end guess-end) (or position 0) (or inner-end 0)))
     (define report-position (or position effective-end))
     (define pb (scheduler-port-broker (parser-job-scheduler job)))
     (define report-line (port-broker-line pb report-position))
     (define report-column (port-broker-column pb report-position))
     (define made-progress? (cond [(eq? #t made-progress?/given) #t]
                                  [(eq? #f made-progress?/given) #f]
                                  [(> (or end position guess-end) start-position) #t]
                                  [else #f]))
     (parse-failure parser
                    message
                    report-line
                    report-column
                    report-position
                    start-position
                    effective-end
                    made-progress?
                    best-failure
                    (and (chido-parse-keep-multiple-failures?)
                         all-failures))]))

(define (format-parser+message parser message)
  (if message
      (format "~a: ~a" (parser-name parser) message)
      ;; TODO - this format should not be necessary, but parser-name is not always returning strings.  I should fix that.
      (format "~a"
              (parser-name parser))))

(define (parse-failure->string/simple pf)
  (format "Parse failure at ~a. ~a"
          (parse-failure->string/location-triple pf)
          (format-parser+message (parse-failure-parser pf)
                                 (parse-failure-message pf))))
(define (parse-failure->string/message pf)
  (format "Parse failure at ~a. ~a"
          (parse-failure->string/location-triple pf)
          (format-parser+message (parse-failure-parser pf)
                                 (parse-failure->first-message pf))))

(define (parse-failure->string/chain pf)
  (format "Parse failure at ~a.\n~a\n"
          (parse-failure->string/location-triple pf)
          (string-join
           (map (λ (pf) (format-parser+message (parse-failure-parser pf)
                                               (parse-failure-message pf)))
                (parse-failure->chain pf))
           "\n")))

(define (parse-failure->string/tree pf)
  (define (->tree depth pf)
    (define (print-indent)
      (for ([i (in-range depth)])
        (printf "  ")))
    (with-output-to-string
      (λ ()
        (print-indent)
        (printf "* ~a\n" (parse-failure->string/message pf))
        (for ([inner (or (parse-failure-inner-failure-list pf) '())])
          (printf "~a\n" (->tree (add1 depth) inner))))))
  (->tree 0 pf))


(define (make-cycle-failure job job-cycle-list)
  (match job
    [(s/kw parser-job #:parser parser #:start-position start-position
           #:scheduler scheduler)
     (define pb (scheduler-port-broker scheduler))
     (parse-failure parser
                    (format "Cycle failure: ~a" (map job->display
                                                     (reverse job-cycle-list)))
                    (port-broker-line pb start-position)
                    (port-broker-column pb start-position)
                    start-position
                    start-position
                    start-position
                    #f
                    #f
                    #f)]))

(define (exn->failure e job)
  (let ([message (format "Exception while parsing: ~a\n" (exn->string e))])
    (match job
      [(s/kw parser-job #:parser parser
             #:start-position start-position
             #:port port
             #:scheduler scheduler)
       (define pb (scheduler-port-broker scheduler))
       (define fail-pos (if port (port->pos port) start-position))
       (parse-failure parser
                      message
                      (port-broker-line pb fail-pos)
                      (port-broker-column pb fail-pos)
                      fail-pos
                      start-position
                      fail-pos
                      (not (equal? start-position fail-pos))
                      #f
                      #f)])))
(define (exn->failure/export e)
  (let ([j (current-chido-parse-job)])
    (if j
        (exn->failure e j)
        (error 'exn->failure
               "Can only be called in the dynamic extent of chido-parse."))))

(define (alt-worker->failure aw)
  (match aw
    [(s/kw alt-worker #:job job #:failures failures)
     (match job
       [(s/kw parser-job #:parser parser #:start-position start-position
              #:scheduler scheduler)
        (define pb (scheduler-port-broker scheduler))
        (match parser
          [(s/kw alt-parser #:name name)
           (parameterize ([current-chido-parse-job job])
             (make-parse-failure
              #:message
              (if (null? failures)
                  "Alt failed with no inner failures.  Probably no prefixes matched."
                  #f)
              #:failures failures))])])]))



#|
Schedulers keep track of parse work that needs to be done and have caches of results.

• job-info->job-cache is a multi-layered cache (of hashes and other data structures) that allows a parser, position, etc to be resolved into a job.

• The done-k-stack is a stack (of continuation-workers) that is pushed to for each non-left-recursive entry to the scheduler.  When that continuation is ready, the scheduler can return.
• The top-job-stack is a stack (of jobs) that is pushed to for each non-left-recursive entry like the done-k-stack.  One of these two should be removed, because they are basically redundant.
• hint-stack-stack - a stack (one for each non-left-recursive entry to the scheduler) of stacks of hints to the scheduler to quickly pick a next job.
|#
(struct scheduler
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
  (
   ;; parser is either a proc-parser struct or an alt-parser struct
   parser
   scheduler
   ;; chido-parse-parameters
   cp-params
   start-position
   result-index
   ;; gvector of siblings indexed by result-index
   ;; IE jobs reference their siblings as the final layer of job caching so
   ;; that you can easily get another (IE the next) job in the series.
   siblings
   [port #:mutable]
   [worker #:mutable]
   ;; dependents are continuation-workers or alt-workers
   [dependents #:mutable]
   ;; The actual result.
   [result #:mutable]
   )
  #:transparent)

(define (job-immediately-actionable? job)
  (and (not (job->result job))
       (match job
         [(s/kw parser-job #:worker c/w)
          (match c/w
            [#f #t]
            [(s/kw continuation-worker #:ready? (? (λ(x)x))) #t]
            [(s/kw alt-worker #:ready-jobs (? (λ(x) (not (null? x))))) #t]
            [(stream-worker result-stream) #t]
            [else #f])])))

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

(struct continuation-worker
  ;; The dependency is only mutated to break cycles.
  (job k [dependency #:mutable] [ready? #:mutable])
  #:transparent)

(struct stream-worker
  (stream)
  #:transparent)


(define (make-continuation-worker-for-job j k dep)
  (continuation-worker j k dep #f))


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
Scheduler cache (IE global cache containing scheduler objects):
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
  (make-gvector #:capacity 10000))
(define (make-parser-cache)
  (make-hasheq))
(define (make-cp-params-cache)
  (make-hash))


(define (get-job-0! s parser cp-params start-position)
  (define (make-fresh-parser-job usable)
    (define siblings-vec (make-gvector))
    (define job
      (parser-job usable s cp-params start-position
                  0 siblings-vec #f #f '() #f))
    (gvector-add! siblings-vec job)
    job)
  (define (traverse-cache parser)
    ;; Traverse the cache.  If no job is found, it updates the cache with a job.
    ;; Either way it returns a job object.
    ;; Each level of the cache may have a literal job object or a layer of caching.
    ;; A layer of caching may be a gvector or a hash table.
    (define (job-match? j)
      (and
       (eq? parser (parser-job-parser j))
       (equal? cp-params (parser-job-cp-params j))))
    (define update #t)
    (inc-traverse-cache!)
    (define c/start-pos (scheduler-job-info->job-cache s))
    (define start-pos-referenced
      (and (< start-position (gvector-count c/start-pos))
           (gvector-ref c/start-pos start-position)))
    (define (start-pos-add-cache-layer! old-job)
      (inc-cache-parser-conflict!)
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
       (define parser-referenced-mutated? #f)
       (define parser-referenced
         (hash-ref! c/parser parser
                    (λ ()
                      (set! parser-referenced-mutated? #t)
                      (make-fresh-parser-job parser))))
       (define (parser-add-cache-layer! old-job)
         (inc-cache-cp-param-conflict!)
         (define new-cache-layer (make-cp-params-cache))
         (define new-job (make-fresh-parser-job parser))
         (hash-set! new-cache-layer (parser-job-cp-params old-job) old-job)
         (hash-set! new-cache-layer cp-params new-job)
         (hash-set! c/parser parser new-cache-layer)
         new-job)
       (match parser-referenced
         [(? parser-job?) (if (or parser-referenced-mutated?
                                  (job-match? parser-referenced))
                              parser-referenced
                              (parser-add-cache-layer! parser-referenced))]
         [else
          (hash-ref! parser-referenced cp-params
                     (λ () (make-fresh-parser-job parser)))])])
    )

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
         (let ([new-job (parser-job parser scheduler cp-params start-position
                                    next-index siblings port #f '() #f)])
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
                 (define sched-k
                   (make-continuation-worker-for-job parent-job k job))
                 (set-parser-job-worker! parent-job sched-k)
                 (push-parser-job-dependent! job sched-k)
                 ;(push-hint! scheduler sched-k)
                 (abort-current-continuation chido-parse-prompt))
               chido-parse-prompt))
            ;; This is the original entry into the parser machinery.
            ;; Or a recursive call that isn't a left-recursion.
            (let ()
              (define result
                (let ()
                  (define k-job (continuation-worker #f #f job #f))
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

  (inc-find-work!)
  (let loop ([stacks (list job-stack)]
             [blocked '()]
             [last-alt-stack #f #;(get-last-alt-stack job-stack)])
    (inc-find-work-loop!)
    (if (null? stacks)
        #f
        (let* ([jstack (car stacks)]
               [j (car jstack)])
          (cond [(memq j blocked) (loop (cdr stacks) blocked last-alt-stack)]
                ;; if it has a worker, add dependencies to jobs
                [else (match (parser-job-worker j)
                        [(s/kw continuation-worker
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
                        [(stream-worker result-stream) jstack]
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
  (define scheduler-done? (continuation-worker-ready? done-k))
  (cond
    [scheduler-done?
     ;; Escape the mad world of delimited-continuation-based parsing!
     (job->result (continuation-worker-dependency done-k))]
    [else
     (inc-run-scheduler!)
     (define using-hint? #t)
     (when (null? (car (scheduler-hint-stack-stack s)))
       (inc-no-hint!)
       (set! using-hint? #f)
       (push-hint! s (car (scheduler-top-job-stack s))))
     (define hint-stack (car (scheduler-hint-stack-stack s)))
     (define hint-worker (parser-job-worker (car hint-stack)))
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
       [(s/kw continuation-worker #:job job #:ready? #t)
        ;(pop-hint! s)
        (run-actionable-job? job)]
       [(s/kw continuation-worker)
        (find-and-run-actionable-job s hint-stack using-hint?)]
       [(s/kw alt-worker #:job job #:ready-jobs (list ready-job rjs ...))
        (run-actionable-job? job)]
       [(s/kw alt-worker #:job job #:remaining-jobs (list))
        (run-actionable-job? job)]
       [(s/kw alt-worker)
        ;; No ready jobs
        (find-and-run-actionable-job s hint-stack using-hint?)]
       [(stream-worker result-stream) (run-actionable-job? (car hint-stack))]
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
       (rec (parser-job-worker next-job) (cons job jobs))]
      [(s/kw continuation-worker #:job job #:dependency dependency)
       (if (memq dependency jobs)
           (begin
             (set-continuation-worker-dependency!
              goal
              (cycle-breaker-job dependency jobs))
             (set-continuation-worker-ready?! goal #t))
           (rec (parser-job-worker dependency)
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
    [(s/kw parser-job #:parser parser #:worker k/worker
           #:result-index result-index
           #:start-position start-position #:cp-params cp-params)
     (match k/worker
       [(s/kw continuation-worker #:job job #:dependency dependency #:k k)
        (do-run!/continuation scheduler
                              job
                              k
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
             (set-parser-job-worker! this-next-job k/worker)
             (set-parser-job-worker! job #f)
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
       [(stream-worker result-stream)
        (do-run!/thunk scheduler
                       job
                       (λ () (stream-rest result-stream)))]
       [#f
        (cond [(equal? 0 result-index)
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
                      (do-run!/thunk
                       scheduler
                       job
                       (λ ()
                         (parameterize ([current-chido-parse-parameters
                                         cp-params])
                           (procedure proc-input))))
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
                  (set-parser-job-worker! job worker)
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
                 (set-parser-job-worker! job #f)
                 (run-scheduler scheduler))])])]))

(define (do-run!/thunk scheduler job thunk)
  (define result
    (call-with-continuation-prompt
     (λ ()
       (parameterize ([current-chido-parse-job job])
         (call-with-continuation-prompt thunk
                                        parse*-direct-prompt)))
     chido-parse-prompt
     recursion-handler))
  (result-check-loop scheduler job result))

(define (do-run!/continuation scheduler job continuation k-arg)
  (define result
    (call-with-continuation-prompt continuation
                                   chido-parse-prompt
                                   recursion-handler
                                   k-arg))
  (result-check-loop scheduler job result))

(define (recursion-handler) recursive-enter-flag)
(define (result-check-loop scheduler job result)
  (cond [(eq? result recursive-enter-flag) (run-scheduler scheduler)]
        [(and (stream? result)
              (not (flattened-stream? result))
              (not (parse-failure? result)))
         (result-check-loop
          scheduler
          job
          (call-with-continuation-prompt
           (λ () (parameterize ([current-chido-parse-job job])
                   (delimit-parse*-direct
                    (stream-flatten result))))
           chido-parse-prompt
           recursion-handler))]
        [else (begin (cache-result-and-ready-dependents! scheduler job result)
                     (run-scheduler scheduler))]))


(define (ready-dependents! job)
  (for ([dep (parser-job-dependents job)])
    (match dep
      [(s/kw alt-worker #:ready-jobs ready-jobs)
       (set-alt-worker-ready-jobs! dep (cons job ready-jobs))]
      [(s/kw continuation-worker)
       (set-continuation-worker-ready?! dep #t)]))
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
  ;; We can delete the worker from a procedural job, because it will never be used again (IE continuation-worker or stream-worker, never alt-worker).
  (set-parser-job-worker! job #f)
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
     (set-parser-job-worker! next-job (stream-worker result))
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


(define default-failure-proc (λ (f) f (make-parse-failure #:inner-failure f)))

(define-syntax (for/parse stx)
  (syntax-parse stx
    [(_ ([arg-name input-stream]
         (~optional (~seq #:failure failure-arg:expr)))
        body ...+)
     #'(for/parse-proc (λ (arg-name) body ...)
                       (λ () input-stream)
                       (~? failure-arg default-failure-proc))]))

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
  (define a1-parser-obj (make-proc-parser #:name "a" #:prefix "" a1-parser-proc))

  (define (Aa-parser-proc port)
    (for/parse
     ([d/A (parse* port (get-A-parser))])
     (for/parse
      ([d/a (parse* port a1-parser-obj
                    #:start d/A)])
      (make-parse-derivation (λ args (string-append (parse-derivation-result! d/A)
                                                    (parse-derivation-result! d/a)))
                             #:derivations (list d/A d/a)))))

  (define Aa-parser-obj (make-proc-parser #:name "Aa" #:prefix "" Aa-parser-proc))


  (define A-parser (make-alt-parser "A"
                                    (list
                                     Aa-parser-obj
                                     a1-parser-obj)))
  (define (get-A-parser) A-parser)

  (define results1 (parse* p1 A-parser))
  (check-equal? (map parse-derivation-result! (stream->list results1))
                (list "a" "aa" "aaa" "aaaa" "aaaaa"))


  (define (Aa-parser-proc/direct port)
    (define d/A (parse*-direct port (get-A-parser/direct)))
    (define d/a (parse*-direct port a1-parser-obj))
    (make-parse-derivation (λ args (string-append (parse-derivation-result! d/A)
                                                  (parse-derivation-result! d/a)))
                           #:derivations (list d/A d/a)))
  (define Aa-parser-obj/direct
    (make-proc-parser #:name "Aa" #:prefix "" Aa-parser-proc/direct))
  (define A-parser/direct (make-alt-parser "A"
                                           (list
                                            Aa-parser-obj/direct
                                            a1-parser-obj)))
  (define (get-A-parser/direct) A-parser/direct)

  (define results2 (parse* p1 A-parser/direct))
  (check-equal? (map parse-derivation-result! (stream->list results2))
                (list "a" "aa" "aaa" "aaaa" "aaaaa"))

  (define c3-parser
    (make-proc-parser #:name "c3-parser" (λ (port) (read-string 3 port))))
  (check-equal?
   (map parse-derivation-result!
        (stream->list
         (parse* (open-input-string "abc")
                 c3-parser)))
   '("abc"))

  (check-equal?
   (map parse-derivation-result!
        (stream->list
         (parse* (open-input-string "◊")
                 "◊")))
   (list "◊"))

  )

