#lang scribble/manual
@(require (for-label racket/base racket/stream chido-parse))
@(declare-exporting chido-parse)

@title{Reference}

NOTE - chido-parse is NOT YET STABLE, and APIs may change.

@section{Gotchas}

Don't use @racket[parameter?]s, use @racket[chido-parse-parameter?]s.
Don't use @racket[stream-cons] or other stream constructors, use @racket[parse-stream-cons] and its derivatives provided by Chido Parse.

@section{Core API}

@subsection{Parsing Functions}

@defproc[(parse* [input (or/c string? input-port?)] [parser parser?]
                 [#:start start (or/c #f number? parse-derivation?) #f])
         stream?]{
  Parse the @racket[input] with @racket[parser].
  If @racket[start] is @racket[#f], start at the current port position (at the beginning if @racket[input] is a string).
  Returns a stream of parse derivations.
  The empty stream element is a @racket[parse-failure?] instead of the vanilla @racket[empty-stream].

  If this is the first call into the parsing system, the input port is actually replaced for inner parses with a wrapper that can be rewound for multiple ambiguous parses.
  In any event, @racket[parse*] does NOT change the current position of the port.
  If you do a series of parses with @racket[parse*], you should pass the previous derivation as the start position to the next call of @racket[parse*].
  Realistically, within a parser @racket[parse*] should only be used in tail position or in combination with @racket[for/parse].
  For a more convenient function for use within parsers, see @racket[parse*-direct].

  This is technically the core parsing function, but you probably want to use @racket[whole-parse] or @racket[whole-parse*] for the initial parse, and @racket[parse*-direct] inside parsers.
}

@defproc[(parse [input (or/c string? input-port?)] [parser parser?])
         (or/c parse-derivation? parse-failure?)]{
  Like @racket[parse*], but:
  @itemlist[
  @item{You can't set the start position: it always starts at the port position.}
  @item{If the result of @racket[parse*] would be a stream with more than one derivation, @racket[parse] returns a parse failure.  IE no ambiguity is allowed.}
  @item{A successful parse moves the port position to the end of the derivation.}
  ]
}

@defproc[(whole-parse* [input (or/c string? input-port?)] [parser parser?]
                       [#:start start (or/c #f number? parse-derivation?) #f])
                       stream?]{
  Like @racket[parse*], but only returns results that consume the entire input (starting at @racket[start] position).
}
@defproc[(whole-parse [input (or/c string? input-port?)] [parser parser?])
         (or/c parse-derivation? parse-failure?)]{
  @racket[whole-parse] is to @racket[whole-parse*] as @racket[parse] is to @racket[parse*].
  Always starts at the current port position, only returns results that consume the entire input, and fails if the result is ambiguous.
}

@defproc[(parse*-direct [input input-port?] [parser parser?]
                        [#:start start (or/c #f number? parse-derivation?) #f]
                        [#:failure failure-arg (or/c #f (-> parse-failure? parse-failure?)) #f])
         parse-derivation?]{
  Can only be used inside a @racket[parser?].
  Like @racket[parse*], but it uses delimited continuation magic.
  The return to the call-site is always a @racket[parse-derivation?].
  However, @racket[parse*-direct] captures the current (delimited) continuation of the parser and returns a stream which applies the continuation to every derivation returned by @racket[parse*].
  The overall return of the parser will be the stream created by @racket[parse*-direct].
  You can use multiple calls to @racket[parse*-direct] within a function and it will nest the streams appropriately.
  When @racket[parse*-direct] returns, the port argument of the parsing procedure will be set to the end-position of the derivation returned, so you don't need to supply start positions to future calls of parse*-direct.

  For example, here is a parser that parses the string @racket["abc"].
  @racketblock[(define abc-parser
                 (proc-parser
                  (λ (port)
                     (define a (parse*-direct port "a"))
                     (define b (parse*-direct port "b"))
                     (define c (parse*-direct port "c"))
                     (make-parse-derivation (string-append
                                             (map parse-derivation-result (list a b c)))
                                            #:derivations (list a b c)))))]

However, if you are doing some OTHER delimited continuation stuff or creating a stream of results using something other than @racket[parse-stream-cons] (or @racket[for/parse]), you may need to take care to use @racket[delimit-parse*-direct] so the overall parse result is correct.

  The @racket[failure-arg] lets you synthesize a custom @racket[parse-failure?], giving the inner failure from the end of the @racket[parse*] stream to your function.
}
@defform[(delimit-parse*-direct expression)]{
  This is automatically done upon executing a parsing procedure and by @racket[parse-stream-cons], as well as by @racket[for/parse].
  If you use something else to construct a result stream, you need to manually use this to prevent @racket[parse*-direct] from aborting out of your stream constructor and returning its own stream in its place.

  But really just don't use something other than @racket[parse-stream-cons] (and its derivatives) to construct result streams!
}

@subsection{Parse Derivations}

@defproc[(make-parse-derivation [result (or/c any/c (-> any/c number? number? number? number? (listof parse-derivation?) any/c))]
                                [#:end end (or/c #f number?) #f]
                                [#:derivations derivations (listof parse-derivation?) '()])
         parse-derivation?]{

Make a parse derivation.
Can only be called during the dynamic extent of a @racket[parser?].
If @racket[end] is #f, it determines the end by calculating the maximum end position of the subderivations supplied.
If no subderivations are supplied and @racket[end] is #f, it raises an error.

Result can be anything, but if it is a procedure it is assumed to be a delayed result.
The procedure should be of the form:

@racketblock[(λ (src line col pos span derivations) (datum->syntax #f 'my-result (list src line col pos span)))]
}

@defproc[(parse-derivation? [x any/c]) bool/c]{
Predicate for parse-derivations.
}
@defproc[(parse-derivation-result [pd parse-derivation?]) any/c]{
Returns the result of a parse-derivation.  May force the supplied procedure, but the result is cached and only forced once.
}
@defproc[(parse-derivation-parser [pd parse-derivation?]) parser?]{
Return the @racket[parser?] that was used to create @racket[pd].
}
@defproc[(parse-derivation-source-name [pd parse-derivation?]) any/c]{
Returns the source name of the input used for parsing.
}
@defproc[(parse-derivation-line [pd parse-derivation?]) (or/c #f number?)]{
Returns the line number of the derivation.
}
@defproc[(parse-derivation-column [pd parse-derivation?]) (or/c #f number?)]{
Returns the column number of the derivation.
}
@defproc[(parse-derivation-start-position [pd parse-derivation?]) (or/c #f number?)]{
Returns the start position number of the derivation.
}
@defproc[(parse-derivation-end-position [pd parse-derivation?]) (or/c #f number?)]{
Returns the end position number of the derivation.
}
@defproc[(parse-derivation-subderivations [pd parse-derivation?]) (listof parse-derivation?)]{
Returns the list of subderivations used in constructing @racket[pd].  Useful for filtering operator precedence and such.
}


@;TODO - these are convenience helpers for filtering
@; parse-derivation-parser?
@; parse-derivation-parser-name?
@; parse-derivation-left-most-subderivation
@; parse-derivation-right-most-subderivation


@subsection{Parse Failures}

@defproc[(make-parse-failure [#:message message (or/c #f string?) #f]
                             [#:position position (or/c number? #f) #f]
                             [#:end end (or/c number? #f) #f]
                             [#:made-progress? made-progress? any/c progress-default]
                             [#:inner-failure inner-failure (or/c parse-failure? #f) #f]
                             [#:failures failures (or/c (listof parse-failure?) #f)#f])
         parse-failure?]{
Make a @racket[parse-failure?].
Can only be called during the dynamic extent of a @racket[parser?].
The @racket[message] and @racket[position] are used to help the user figure out what went wrong.
Inner failures are also used to improve the message, and provide a chain of failures.
Everything aside from the message is used in comparing parse failures when there are ambiguous failures to try to find the best failure message to display to the user.

When the @racket[chido-parse-keep-multiple-failures?] is true, all inner failures are kept (IE @racket[inner-failure] and @racket[failures]), though there isn't currently a very useful way of looking at them.
When @racket[chido-parse-keep-multiple-failures?] is false, only the best inner failure is kept.
If @racket[inner-failure] is supplied, it is always used as the best inner failure.
If @racket[inner-failure] is not supplied, a heuristic sorting function is used on @racket[failures] to choose one.
Of course, a terminal parser will have no inner failures, and that's fine too.
}
@defproc[(parse-failure? [x any/c]) bool/c]{
Predicate for parse failure objects.

Note that parse failures implement the @racket[gen:stream] interface as empty streams.
}

@defproc[(exn->failure [e exn?]) parse-failure?]{
Convenience function for turning an exception into a parse failure.
This is not done automatically -- an unhandled exception during parsing will kill the entire parser.
But if you want to run an existing parsing function that raises exceptions upon failure, catch the exception and use this.
}

@defproc[(parse-failure->string/location-triple [pf parse-failure?]) string?]{
Returns the location triple of a parse failure as a string.  Eg. @tt{<line>:<column>:<position>}.
}
@defproc[(parse-failure->string/simple [pf parse-failure?]) string?]{
Returns a string for the parse failure.  Includes location triple, name of the parser that failed, and the message if one is on the failure object.
}
@defproc[(parse-failure->string/message [pf parse-failure?]) string?]{
Like @racket[parse-failure->string/simple], but searches the failure chain to find the first message if one is not on the outermost failure.
}
@defproc[(parse-failure->string/chain [pf parse-failure?]) string?]{
Formats the list of failures that cascaded into the creation of @racket[pf], showing the info from @racket[parse-failure->string/simple] for each.
}
@defproc[(parse-failure->string/tree [pf parse-failure?]) string?]{
Like @racket[parse-failure->string/chain], except it shows ALL failures, not just the chain of best failures.
It indents each message to show the tree relationship of failures.
This is only useful if @racket[chido-parse-keep-multiple-failures?] is set to true.
Be prepared for a lot of text.
}

@; TODO - this is provided but not documented.
@;greatest-failure

@defparam[chido-parse-keep-multiple-failures? keep any/c #:value #f]{
Parameter used to determine whether to store all sub-failures within a failure object.
Keeping all failures is mostly an academic exercise.
}

@subsection{Chido Parse Parameters}

@defproc[(chido-parse-parameter? [x any/c]) any/c]{
Predicate for chido-parse-parameters.

Don't use @racket[parameter?]s inside parsing procedures.
Use chido-parse-parameters instead.

They behave similarly, but chido-parse-parameters are a key to the parsing cache (so you don't accidentally pull out a cached result that was constructed by referring to a @emph{different} parameter value, such as a different value for @racket[current-chido-readtable]) and are preserved by @racket[parse-stream-cons], whereas regular parameters are NOT preserved by @racket[stream-cons] and friends.
}

@defproc[(chido-parse-parameter [default any/c]) chido-parse-parameter?]{
Construct a chido parse parameter with the given default value.
}
@defform[(chido-parse-parameterize ([cp-param value] ...) body ...)]{
Like @racket[parameterize], but for @racket[chido-parse-parameter?]s.
}

@subsection{Stream construction}

@defform[(parse-stream-cons head tail)]{
Use this instead of @racket[stream-cons].
Like @racket[stream-cons], but plays nicely with @racket[chido-parse-parameters] and @racket[parse*-direct].
}
@defform[(for/parse ([binding-id stream-expression]) body ...)]{
Similar to other @racket[for] forms.
But it returns a stream created using @racket[parse-stream-cons] and the @racket[stream-expression] must return a stream.
}


@subsection{Parsers and core parser constructors}

@defproc[(parser? [x any/c]) any/c]{
Don't really use this.
This should maybe be replaced by a higher-order contract.
Right now it's just a first-order check that things seem OK.

BUT - here I'll at least document what this is meant to check, and what you can expect from functions that return @racket[parser?]s.

Parsers are either @racket[proc-parser]s, @racket[alt-parser]s, @racket[string?]s, structs that implement @racket[prop:custom-parser], @racket[non-cached-parser-thunk], or a thunk that returns one of those.

Thunks are allowed because parsers often need to recursively refer to each other.
If parsers are mutually recursive, you have an ordering issue where neither can be constructed without the other.
To break the cycle, put one in a thunk!
Thunks that contain parsers are only forced once and cached, so you can rely on the results of the thunk being @racket[eq?] (eg.for filtering purposes).
Because sometimes you really do want the thunk to be re-evaluated, thunks wrapped with the @racket[non-cached-parser-thunk] struct are NOT cached.
Additionally, @racket[chido-parse-parameter?]s used as parser thunks are not cached, because their whole point is to be this dynamically extensible thing to allow you to implement parsers independently then have them dynamically reference the right parser when recurring.

If you use a procedure as a parser that is NOT a thunk that returns (perhaps eventually through multiple layers of thunks) a primitive parser, you will get an exception when the scheduler actually tries to force it and use it as a parser.
}

@defproc[(proc-parser
          [proc (-> input-port? (or/c any/c stream?))]
          [#:prefix prefix string? ""]
          [#:name name string? (format "~a" (object-name proc))]
          [#:preserve-prefix? preserve-prefix? any/c #f]
          [#:promise-no-left-recursion? promise-no-left-recursion? any/c #f])
         proc-parser?]{
Make a procedural parser.
The procedure should accept an input port.
It should return a @racket[parse-derivation?] or @racket[parse-failure?].
If it's an ambiguous parser (or recurs with @racket[parse*], therefore inheriting any ambiguity of the recursive parser used), it should return a stream tree that ultimately contains @racket[parse-derivation?]s and @racket[parse-failure?]s.

If @racket[prefix] is given, it is a literal prefix to match before the procedure is called.
Unless @racket[preserve-prefix?] is true, the port is set to just @emph{after} the prefix, so your parsing function can focus on the interesting part of the parse.
@racket[preserve-prefix?] is mostly there as a convenience for wrapping existing parsing procedures.

The @racket[prefix] may be queried and used in a semantically meaningful way, as by @racket[chido-readtable?]s.

Parsers with prefixes (that don't use @racket[preserve-prefix?]) are statically known not to be left-recursive.
The scheduler is optimized to try non-left-recursive parsers first.
In RacketCS continuation capture is really cheap, but it's somewhat expensive in RacketBC, so an additional optimization is that parsers that are marked as non-left-recursive DON'T get their continuations captured and aborted when recurring!
If you @emph{really} promise that your procedure is not left-recursive, you can mark it as such with @racket[promise-no-left-recursion].
But if you lie your parser might go into an infinite loop!
}
@defproc[(proc-parser? [p any/c]) any/c]{
Predicate for parsers constructed with @racket[proc-parser].
If this is true, @racket[parser?] is also true.
}
@defproc[(alt-parser [#:name name (or/c string? #f) #f] [p parser?] ...) alt-parser?]{
Construct a parser that returns the results of all of the given parsers.
IE it's an alternation combinator.
}
@defproc[(alt-parser? [p any/c]) any/c]{
Predicate for parsers constructed with @racket[alt-parser].
If this is true, @racket[parser?] is also true.
}

@defproc[(non-cached-parser-thunk [t (-> parser?)]) parser?]{
For when you want the parser to be computed fresh every time you use it.
Note that if the thunk returns ANOTHER thunk, the inner thunk WILL be cached.
}

@defthing[prop:custom-parser struct-type-property?]{
You can define your own structs that act as parsers.

TODO - I should document this.  But really it should probably be gen:custom-parser and support multiple methods, eg. for name, prefix, etc.

IE this is not stable.

At any rate, while this is undocumented you can also just use a function that takes your struct and returns a parser, though that's less satisfying.
}

@defproc[(parser-name [p parser?]) string?]{
Get the name of a parser.

Note that this has to force thunks.

If you try to use sub-parsers when constructing the name of a parser, make sure you don't have a mutually recursive cycle going on, or you will have a bad time.
}
@defproc[(parser-prefix [p parser?]) string?]{
Get the prefix of a parser.

Note that this has to force thunks.
}
@defproc[(parser-potentially-left-recursive? [p parser?]) string?]{
If you are making a combinator, you might want to use this to determine whether your whole result will be left recursive and use it when constructing a @racket[proc-parser].

Note that this has to force thunks.
}
@defproc[(parser-potentially-null? [p parser?]) string?]{
If you are making a combinator, you might want to use this to determine whether your whole result will be left recursive and use it when constructing a @racket[proc-parser].

Note that this has to force thunks.
}

@section{Literal Parsers}

You can use @racket[string?]s as literal parsers.
You can always create @racket[proc-parser]s that parse literals.
The other literal parsers here are just @racket[proc-parser]s that are convenient enough to be in the standard parser library.

@defthing[eof-parser parser?]{
Parser that succeeds when reading a port would return an @racket[eof-object?].
}

@defthing[any-char-parser parser?]{
A parser that parses any single character.
}


@section{Combinators}

The alternation combinator @racket[alt-parser] is documented in another section.

@defproc[(sequence [#:name name (or/c #f string?) #f]
                   [#:derive derive procedure? #f]
                   [#:result/bare make-result/bare procedure? #f]
                   [#:result/stx make-result/stx procedure? #f]
                   [#:between between (or/c #f parser?) #f]
                   [#:before before (or/c #f parser?) #f]
                   [#:after after (or/c #f parser?) #f]
                   [parser parser?] ...)
         parser?]{
Sequence combinator.
You can add optional parsers between members of the sequence with @racket[between].
The @racket[between] parser is @emph{not} put between the first and last normal parsers and the @racket[before] and @racket[after] parsers.
For example @racket[(sequence #:before "<" "a" "b" #:after ">" #:between "-")] would parse @racket["<a-b>"] but not @racket["<-a-b->"].

The @racket[derive], @racket[result/bare], and @racket[result/stx] arguments are for building the result derivation, and only one may be given (a non-@racket[#f] value).
They are all procedures that must accept one value for each of @racket[parser], but @emph{not} the @racket[before], @racket[between], or @racket[after] parsers.

@racket[derive] should be a function that accepts one derivation for each @racket[parser], and should return a @racket[parse-derivation?].

@racket[result/bare] should be a function that accepts the result values (IE using @racket[parse-derivation-result]) from each @racket[parser] and should return a value to be used as the result field of @racket[make-parse-derivation].
You can also provide @racket[#t] instead of a procedure to just get the results in a list.

@racket[result/stx] is like @racket[result/bare], but it wraps the result you return as a syntax object with appropriate source location info.
You can also provide @racket[#t] instead of a procedure to just get the results in a syntax list.

If none of the result options are given, the default is as if @racket[result/stx] is @racket[#t].

TODO - example usage.
}

@defproc[(repetition [#:name name (or/c #f string?) #f]
                     [#:derive derive procedure? #f]
                     [#:result/bare make-result/bare procedure? #f]
                     [#:result/stx make-result/stx procedure? #f]
                     [#:min min number? 0]
                     [#:max max number? +inf.0]
                     [#:greedy? greedy? any/c #f]
                     [#:between between (or/c #f parser?) #f]
                     [#:before before (or/c #f parser?) #f]
                     [#:after after (or/c #f parser?) #f]
                     [parser parser?])
         parser?]{
Repetition combinator.
Parse @racket[parser] repeatedly.

The optional argument API is like @racket[sequence].
@racket[between] is used between iterations of @racket[parser], @racket[before] and @racket[after] are parsed before/after the first/last repetition of @racket[parser].

@racket[derive], @racket[result/bare], and @racket[result/stx] act like @racket[sequence] but should accept a single argument which will be a list of derivations returned by @racket[parser] (in order).
Again, @racket[result/bare] and @racket[result/stx] may be @racket[#t] instead of a procedure, in which case the results will be put in a list or syntax list.
If none of the options are non-false, @racket[result/stx] is treated as @racket[#t].

You can specify the minimum and maximum number of acceptible repetitions with @racket[min] and @racket[max].
If @racket[greedy?] is non-false, derivations will only be returned if they parsed the @racket[max] number of repetitions or no more repetitions can be parsed after the last one.
Note that the result may still be ambiguous depending on whether @racket[parser] or @racket[before]/@racket[between]/@racket[after] are ambiguous.
If @racket[greedy?] is false, all derivations with a number of repetitions between @racket[min] and @racket[max], inclusive, are returned.
Frankly, for any realistic use, you want @racket[greedy?] to be true, because the extra ambiguity of non-greedy makes a lot of work for the system, even though most of it is immediately thrown away.

TODO - usage example.

UNSTABLE - I might change the default option for @racket[greedy?].  On the one hand, false gives the more general parser and what most people would probably expect at first.  But non-greedy repetition is just so much slower that any practical use really needs to reach for greedy.
}

@defproc[(kleene-star  [#:name name (or/c #f string?) #f]
                       [#:derive derive procedure? #f]
                       [#:result/bare make-result/bare procedure? #f]
                       [#:result/stx make-result/stx procedure? #f]
                       [#:greedy? greedy? any/c #f]
                       [#:between between (or/c #f parser?) #f]
                       [#:before before (or/c #f parser?) #f]
                       [#:after after (or/c #f parser?) #f]
                       [parser parser?])
         parser?]{
Like @racket[repetition] with min 0 and max infinity.

UNSTABLE - I might change the default option for @racket[greedy?].
}
@defproc[(kleene-plus  [#:name name (or/c #f string?) #f]
                       [#:derive derive procedure? #f]
                       [#:result/bare make-result/bare procedure? #f]
                       [#:result/stx make-result/stx procedure? #f]
                       [#:greedy? greedy? any/c #f]
                       [#:between between (or/c #f parser?) #f]
                       [#:before before (or/c #f parser?) #f]
                       [#:after after (or/c #f parser?) #f]
                       [parser parser?])
         parser?]{
Like @racket[repetition] with min 1 and max infinity.

UNSTABLE - I might change the default option for @racket[greedy?].
}
@defproc[(kleene-question  [#:name name (or/c #f string?) #f]
                           [#:derive derive procedure? #f]
                           [#:result/bare make-result/bare procedure? #f]
                           [#:result/stx make-result/stx procedure? #f]
                           [#:greedy? greedy? any/c #f]
                           [#:between between (or/c #f parser?) #f]
                           [#:before before (or/c #f parser?) #f]
                           [#:after after (or/c #f parser?) #f]
                           [parser parser?])
         parser?]{
Like @racket[repetition] with min 0 and max 1.

UNSTABLE - I might change the default option for @racket[greedy?].
}


@defproc[(permutation [#:name name (or/c #f string?) #f]
                      [#:derive derive procedure? #f]
                      [#:result/bare make-result/bare procedure? #f]
                      [#:result/stx make-result/stx procedure? #f]
                      [#:between between (or/c #f parser?) #f]
                      [#:before before (or/c #f parser?) #f]
                      [#:after after (or/c #f parser?) #f]
                      [parser parser?] ...)
         parser?]{
Permutation combinator.
The interface is essentially the same as @racket[sequence], but the parser accepts all permutations of the sequence of @racket[parser]s.
}

@defproc[(epsilon-parser [#:name name string? "epsilon"]
                         [#:result result (or/c #f any/c) #f])
         parser?]{
Parser that parses the empty string.
You can provide a custom name and result (IE the result for @racket[make-derivation]).
}

@defproc[(not-parser [parser parser?]
                     [#:name name string? (format "not_~a" (parser-name parser))]
                     [#:result result any/c #f])
         parser?]{
Create a parser that succeeds when @racket[parser] fails.
In other words, if the result of @racket[parser] is a stream with any derivations, the not-parser fails.
If the result of @racket[parser] is a parse failure (IE empty stream), this parser succeeds.

You can supply @racket[result] as the result field for @racket[make-parse-derivation].
}

@defproc[(peek-parser [parser parser?]) parser?]{
Return a parser that parses with @racket[parser] but wraps the resulting derivation in another derivation whose end point is its start point.
The wrapped derivation will have the same result as the inner derivation returned by @racket[parser].

You could use this with eg. @racket[parse*-direct] to peek ahead without actually effecting the input port.
}

@;@defproc[(char-parser [c (or/c char? string?)]) parser?]{
@;Make a parser that parses a character.
@;I'm not really sure why I wrote this.
@;You can also just use a literal string.
@;}

@defproc[(char-range-parser [min (or/c char? string?)]
                            [max (or/c #f char? string?) #f])
         parser?]{
Parses characters in the given range.
You can supply a string of length 2 for @racket[min] and don't provide @racket[max], or you can supply two characters or strings of length 1.
The first character must have a lower code point value than the second.
}

@defproc[(regexp->parser [rx regexp?] [#:name name (or/c #f string?) #f]) parser?]{
Returns a parser that parses a regexp.
The derivation's result field is the result returned by @racket[regexp-match].
}


@defproc[(as-syntax [p parser?]) parser?]{
Returns a parser whose derivation result is the same as @racket[p]'s, but wrapped up as a syntax object with appropriate source location info.
}

@defproc[(wrap-derivation [p parser?]
                          [wrap-func (-> parse-derivation? parse-derivation?)]
                          [#:name name (or/c #f string?) #f])
         parser?]{
Creates a parser that applies @racket[wrap-func] to the derivations returned by @racket[p].
}

@; TODO - I don't really like this one.
@;traditional-read-func->parse-result-func


@defform[(binding-sequence elem ...
                           global-option ...)
#:grammar
[(elem parser
       [: parser elem-optional ...])
 (global-option (code:line #:name parser-name)
                (code:line #:derive derive)
                (code:line #:result/bare result/bare)
                (code:line #:result/stx result/stx)
                (code:line #:splice splice-global)
                (code:line #:inherit-between inherit-between)
                (code:line #:between between-parser))
 (elem-optional (code:line #:bind binding-name)
                (code:line #:ignore ignore)
                (code:line #:splice splice-number)
                (code:line #:repeat-min repeat-min)
                (code:line #:repeat-max repeat-max)
                (code:line #:repeat-greedy? repeat-greedy?))]]{
Produce a sequence parser that can bind intermediate results and reference them in parsers later in the sequence.
Each element is evaluated to construct parsers @emph{after} the previous parser has returned a derivation, so @racket[binding-sequence] can be used to construct data-dependent parsers.

To bind the derivation as a result, use the @racket[#:bind] keyword to bind the derivation to @racket[name].
Then just refer to @racket[name] later!

Global options affect the entire sequence.
@racket[between-parser] is inserted between each element in the sequence.
If @racket[between-parser] is not specified but @racket[inherit-between] is truthy, then the sequence inherits the @racket[between-parser] from any @racket[binding-sequence] that surrounds this one.
If @racket[splice-global] is a number greater than 0, the resulting list is spliced that many times (which is only useful if your sequence only has a single non-ignored element).
The @racket[derive], @racket[result/bare], and @racket[result/stx] arguments are similar to the similar arguments in @racket[sequence], though note that splices and ignores affect the list of arguments that will be passed to them.
The default derivation result will be a syntax list of the results of the elements (taking into account splices and ignores).

Each element in the sequence may have extra options about binding, splicing, or repetition.
The @racket[binding-name] argument is bound to the derivation of that element for future elements in the sequence.
The @racket[splice-number] argument should be a positive integer, and the result list of that parser will be spliced that number of times into the outer result list.
If the @racket[ignore] argument is true, that element is not included in the result list.
The repetition arguments act as in @racket[repetition], though the @racket[between-parser] is inserted between repetitions.
}

@subsection{Filters}

@defproc[(parse-filter [p parser?]
                       [filter-func (-> port derivation (or/c any/c parse-derivation?))]
                       [#:replace-derivation? replace-derivation? any/c #f]
                       [#:include-failures? include-failures? any/c #t])
         parser?]{
This is probably not your go-to filter combinator, but it is the most general.

The @racket[filter-func] receieves the input port at the position @emph{after} the parser's derivation, as well as the derivation itself.
If the @racket[filter-func] returns false, that derivation is removed from the result stream of the filtered parser.
If @racket[replace-derivation?] is true, the @racket[filter-func]'s non-false results must be new parse derivations, which replace the original derivations in the parse stream.
Otherwise the original parse derivations are used even if the truthy value returned by @racket[filter-func] is itself a parse derivation.

If @racket[include-failures?] is true, each filtered derivation will be replaced in the stream by a @racket[parse-failure?] with a message about the filtered derivation.
It's really only useful if you want to track all failures with @racket[chido-parse-keep-multiple-failures?].
}

@defproc[(not-follow-filter [main-parser parser?] [not-follow-parser parser?]
                            [#:include-failures? include-failures? any/c #t])
         parser?]{
Filters out derivations if @racket[not-follow-parser] can succeed immediately after the derivation.

Eg. @racket[(not-follow-filter "abc" "d")] parses the strings @racket["abc"] and (the first three characters of) @racket["abce"] but not the string @racket["abcd"]
}
@defproc[(must-follow-filter [main-parser parser?] [must-follow-parser parser?]
                             [#:include-failures? include-failures? any/c #t])
         parser?]{
Filters out derivations if @racket[must-follow-parser] can not succeed immediately after the derivation.

Eg. @racket[(must-follow-filter "abc" "d")] parses the first three characters of the string @racket["abcd"], but does not parse the strings @racket["abc"] or @racket["abce"].
}

@defproc[(derivation-filter [p parser?]
                            [filter-func (-> parse-derivation? (or/c any/c parse-derivation?))]
                            [#:replace-derivation? replace-derivation? any/c #f]
                            [#:include-failures? include-failures? any/c #t])
         parser?]{
Filter out derivations that don't pass @racket[filter-func], also replace the derivations if @racket[replace-derivation?] is true.
}

@defproc[(result-filter [p parser?]
                        [filter-func (-> any/c any/c)]
                        [#:replace-result? replace-result? any/c #f]
                        [#:include-failures? include-failures? any/c #t])
         parser?]{
Filter out derivations whose @racket[derivation-result] does not pass @racket[filter-func].
If @racket[replace-result?] is true, derivations are replaced with derivations whose result is the value returned by @racket[filter-func].
}











@section[#:tag "chido-readtables"]{Chido-Readtable Parsers}

Chido-readtables have an interface similar to Rackets readtables (see @secref["readtable" #:doc '(lib "scribblings/guide/guide.scrbl")] in the Racket Guide)

Chido-readtables are a fancy kind of alternative parser.
Among their alternatives, chido-readtables have a built-in symbol parser whose behavior is modified by the other parsers.
Chido-readtables are extended with parsers and a symbol denoting an effect on the built-in symbol parser, such as @racket['terminating] or @racket['nonterminating-layout].

When the built-in symbol parser is used by a chido-readtable, the symbol continues until reaching the prefix of a @racket['terminating] or @racket['terminating-layout] parser or a successful parse from a @racket['soft-terminating] or @racket['soft-terminating-layout] parser.

Layout parsers are whitespace or comment parsers that delimit symbols and are generally allowed between other parsers inside lists.
Results from layout parsers are ignored when creating parse results from readtable parsers, only the fact that they succeed at parsing is relevant.
They come in @racket['terminating-layout], @racket['soft-terminating-layout], and @racket['nonterminating-layout] varieties, which match the symbol effects of @racket['terminating], @racket['soft-terminating], and @racket['nonterminating] parsers, respectively.

Terminating parsers are parsing alternatives that also delimit symbols (IE they terminate the built-in symbol parsing).
In other words no space is necessary between a symbol and a following terminating parser.
@racket['soft-terminating] parsers terminate a symbol only of they parse successfully.
Hard terminating (@racket['terminating]) parsers terminate a symbol when their prefix matches, whether or not they parse successfully.
Therefore the prefixes of hard terminating parsers are disallowed anywhere within symbols.

Nonterminating parsers are alternatives that do not delimit a symbol.
Layout (whitespace) is required between a symbol and a following nonterminating parser.

Terminating and nonterminating parsers can follow each other with no layout in between, though layout is allowed between them, and usually good style to have.

Note that when using a chido-readtable as a parser directly, it parses only ONE form, equivalent to using @racket[chido-readtable->read1].  When you want to parse a sequence using chido-readtables, instead of using a kleene star combinator, you should use @racket[chido-readtable->read*].

Ambiguous results are possible from parsing with a chido-readtable when terminating/nonterminating parsers themselves return ambiguous results or when multiple terminating/nonterminating parsers are successful at a given position.
If another parser is successful, the symbol parser is not tried.
The symbol parser never returns an ambiguous result.

One final parser flag is @racket['left-recursive-nonterminating].
Parsers with the @racket['left-recursive-nonterminating] flag are run @emph{after} the symbol parser and do not effect symbol parsing.
While chido-parse doesn't normally need a flag to handle left-recursive parsers, because most readtable parsers are run before the symbol parser and used to effect the symbol parser, left-recursive parsers need a special flag in the readtable parser.

@subsection{chido-readtable objects}

@defthing[current-chido-readtable chido-parse-parameter?]{
A @racket[chido-parse-parameter] holding a @racket[chido-readtable?].
Use this in any parser that needs to recur by parsing a chido readtable.
Using it allows extension parsers to be written independently and combined while allowing each one to reference the combined readtable, rather than having to re-construct each extension with a reference to the proper extended readtable.
}

@defthing[empty-chido-readtable chido-readtable?]{
A @racket[chido-readtable?] with NO parsers of ANY kind attached, except the built-in symbol parser.

TODO - list the flags that the table starts with, eg. @racket[chido-readtable-symbol-support?].
}


@defproc[(chido-readtable? [x any/c]) any/c]{
Predicate for chido-readtables.
Note that if @racket[chido-readtable?] is true, then @racket[parser?] is true too.
When used directly as parsers, chido-readtables parse once with no leading or trailing layout.
To properly parse multiple times with a chido-readtable, use @racket[chido-readtable->read*] to use the readtable's layout configuration.
}

@subsection{Variations on chido-readtable parsers}

@defproc[(chido-readtable->read1 [rt chido-readtable?]) parser?]{
Returns an opaque parser that parses a single form using @racket[rt] (with NO leading whitespace allowed).
The parser is equivalent to using the readtable directly as a parser.
}
@defproc[(chido-readtable->read1/layout [rt chido-readtable?]) parser?]{
Like @racket[chido-readtable->read1], but with leading layout (using the layout parsers within the readtable).
}
@defproc[(chido-readtable->read* [rt chido-readtable?]) parser?]{
Returns an opaque parser that parses a sequence of forms using @racket[rt], returning a list.
This is preferable to using a chido-readtable directly inside a kleene-star combinator because it consistently handles layout parsing and allows trailing layout.
The parser allows leading, middle, and trailing layout per the layout parser configuration in @racket[rt], and can successfully parse an empty list of forms (which may still have layout).

Note that this does NOT parse parentheses at the start/end of a list -- rather, this is a procedure that could be used on the inside of a list.
Rather, if you want to parse several (potentially extended) s-expressions within a file that potentially has layout (whitespace) before, between, and after the forms, you would use this parser.
Of course, you can also make a list parser with @racket[(sequence "(" (chido-readtable->read* rt) ")")].
But @racket[chido-readtable-add-list-parser] also handles the extra details of adding the list parser to the readtable for recursive parses, adding a terminating and failing parser for the right delimiter, properly using the @racket[current-chido-readtable], and other conveniences, so probably just use it instead.
}
@defproc[(chido-readtable->read+ [rt chido-readtable?]) parser?]{
Like @racket[chido-readtable->read*-parser] but it requires at least one form.
In other words, it fails on empty lists.
}
@defproc[(chido-readtable->layout1 [rt chido-readtable?]) parser?]{
Returns an opaque parser that parses a single piece of layout using @racket[rt].
}
@defproc[(chido-readtable->layout* [rt chido-readtable?]) parser?]{
Returns an opaque parser that parses any amount of layout using @racket[rt].
}
@defproc[(chido-readtable->layout+ [rt chido-readtable?]) parser?]{
Returns an opaque parser that parses one or more pieces of layout using @racket[rt].
}

@defthing[current-chido-readtable-read1-parser parser?]{
Parser that applies @racket[chido-readtable->read1] to @racket[current-chido-readtable].
}
@defthing[current-chido-readtable-layout*-parser parser?]{
Parser that applies @racket[chido-readtable->layout*] to @racket[current-chido-readtable].
}
@defthing[current-chido-readtable-layout+-parser parser?]{
Parser that applies @racket[chido-readtable->layout+] to @racket[current-chido-readtable].
}

@defthing[current-chido-readtable-symbol-parser parser?]{
Parses symbols using the symbol parser of @racket[current-chido-readtable].
IE it uses all the extensions to determine what symbols can be parsed, but it is @emph{ONLY} the symbol parser.

UNSTABLE - chido readtables don't yet support symbols with escapes (IE with backslash) or as literals with pipes around them as Racket's built-in readtable does.
At some point there will be options to set on the readtable itself to toggle options about those.
}

@subsection{Extending and modifying chido-readtables}

All readtable “modifiers” are functional.
They do not actually mutate a readtable, they merely return a new modified readtable.

@defthing[chido-readtable-symbol-effect/c contract?]{
The same as
@racketblock[(or/c 'terminating
                   'soft-terminating
                   'nonterminating
                   'terminating-layout
                   'soft-terminating-layout
                   'nonterminating-layout
                   'left-recursive-nonterminating)]
}

@defproc[(extend-chido-readtable [mode chido-readtable-symbol-effect/c]
                                 [parser parser/c]
                                 [rt chido-readtable?]
                                 [#:operator operator-type
                                 (or/c #f 'infix 'prefix 'postfix) #f]
                                 [#:precedence-less-than precedence-less-than
                                 (or/c any/c (listof any/c)) #f]
                                 [#:precedence-greater-than precedence-greater-than
                                 (or/c any/c (listof any/c)) #f]
                                 [#:associativity associativity
                                 (or/c #f 'left 'right) #f])
         chido-readtable?]{
Extend @racket[rt] with @racket[parser] and given @racket[mode].
To understand the various modes, see @secref["chido-readtables"].

Also, chido-readtables support operators.
You can use the optional arguments to do so, but better yet see @racket[chido-readtable-add-mixfix-operator].
}

@defproc[(extend-chido-readtable* [rt chido-readtable?]
                                  [arg any/c] ...)
         chido-readtable?]{
Applies @racket[extend-chido-readtable] multiple times.
Eg.
@racketblock[(extend-chido-readtable* my-rt
                                      'terminating at-reader-parser
                                      'nonterminating some-other-parser)]
}

@defproc[(chido-readtable-add-list-parser
          [l-delim string?]
          [r-delim string?]
          [rt chid-readtable?]
          [#:wrapper wrapper (or/c #f symbol? (-> syntax? syntax?)) #f]
          [#:inside-readtable inside-readtable
           (or/c #f chido-readtable? (-> chido-readtable?)) #f]
          [#:readtable-symbol-effect readtable-symbol-effect
           chido-readtable-symbol-effect/c 'terminating])
          chido-readtable?]{
Adds a list parser with the given delimiters.
Eg. the default parenthesis parser in Racket is similar to
@racketblock[(chido-readtable-add-list-parser "(" ")" my-rt)].

If @racket[inside-readtable] is @racket[#f], the @racket[current-readtable] is queried for recursive parses (of both forms and layout).
You can instead set @racket[inside-readtable] to a particular readtable to use it, or to a thunk that provides a readtable, in which case the thunk will be called (with no caching) to get an inner readtable each time.

If @racket[wrapper] is a symbol, that symbol is prepended to the list.
Eg. if you use
@racketblock[(chido-readtable-add-list-parser "$(" ")" mostly-normal-rt #:wrapper '#%dollar-paren)],
then the string @racket["$(foo bar)"] would be parsed as @racket['(#%dollar-paren foo bar)].

You can also use a procedure for @racket[wrapper], in which case the procedure takes the result syntax as an argument and must return a syntax object.

Note that if @racket[l-delim] and @racket[r-delim] are the same character, you can't nest lists.
It's a constant annoyance to me that people ever choose to use the same (likely single character) string as both left and right delimiters for things, because of course you can't nest such things without some kind of escaping.

The @racket[readtable-symbol-effect] should generally be @racket['terminating], but another reasonable choice is @racket['terminating-layout] to get an s-expression comment form.
I'm not sure it's a terribly useful comment form, but it's interesting at least.

UNSTABLE - these list forms do not yet support dots for improper lists or for operator moving (a la @racket['(l . op . r)]).
I will eventually add them as optional keyword arguments (without using the keyword, no dots will be supported).
}

@defproc[(chido-readtable-add-raw-string-parser
          [l-delim string?]
          [r-delim string?]
          [rt chid-readtable?]
          [#:wrapper wrapper (or/c #f symbol? (-> syntax? syntax?)) #f]
          [#:readtable-symbol-effect readtable-symbol-effect
           chido-readtable-symbol-effect/c 'terminating])
         chido-readtable?]{
Adds a parser for raw strings with balanced inner delimiters.
In other words, there are no escapes like @racket["\n"], every character in the source string exists in the parsed string, including newlines and embedded delimiters.
If @racket[l-delim] appears in the string before @racket[r-delim], it will cause an @racket[r-delim] to be part of the string instead of the string terminator.
This means that not all strings are representable with a given raw string parser added with this procedure.
But if you really need a string that contains the same delimiters without being balanced, you can use a different string notation.

Raw strings are convenient for embedding source code as strings.
For example, if we use
@racketblock[(chido-readtable-add-raw-string-parser "«" "»" my-rt)],
we can parse the following as a single string:

TODO - this example is messing up my auto-indentation, so I'm commenting it out for the moment.
@;@verbatim{
@;«
@;(print «printf("%d", 5)»)
@;»
@;}

I mean, that's a pretty dumb example, but I think you get the picture.
When you have to embed HTML inside Javascript inside PHP inside ... escaping is super annoying.
But realistically raw strings of source code are useful in Racket in particular because you can write macros to parse those string at compile time.
And sometimes you just want to write strings that contain quotes or backslashes without a bunch of escaping nonsense.
In other words, they are also nice for writing regular expressions (IE a special case of embedding source code as a string).

Note that if @racket[l-delim] and @racket[r-delim] are the same character, you can't nest raw strings.
It's a constant annoyance to me that people ever choose to use the same (likely single character) string as both left and right delimiters for things, because of course you can't nest such things without some kind of escaping.

The @racket[readtable-symbol-effect] should generally be @racket['terminating], but another reasonable choice is @racket['terminating-layout] to get a nestable multi-line comment form.
}

@defproc[(chido-readtable-add-mixfix-operator
          [name (or/c symbol? string?)]
          [rt chido-readtable?]
          [#:layout layout (or/c 'required 'optional 'none) 'required]
          [#:precedence-greater-than precedence-greater-than list? '()]
          [#:precedence-less-than precedence-less-than list? '()]
          [#:associativity associativity (or/c #f 'left 'right) #f])
         chido-readtable?]{
Yes, that's right, chido-readtables can have infix, prefix, and postfix operators.
Whether or not that's a good idea you can decide for yourself.
But they're supported!

This code:
@racketblock[(chido-readtable-add-mixfix-operator "_<+>_" my-rt)]
adds a @tt{<+>} operator to the readtable, so you could write eg:
@racket["(string-length foo) <+> (some-thunk)"]
and it would be read as:
@racket['(#%chido-readtable-infix-operator <+> (string-length foo) (some-thunk))].

The @racket[name] must have an underscore everywhere you want a recursive parse to happen, thus determining whether the operator is infix, prefix, postfix, or “notfix” (meaning that it neither begins nor ends with a recursive parse).
The fixity determines the implicit symbol that starts the result form, either @racket['#%chido-readtable-infix-operator], @racket['#%chido-readtable-prefix-operator], @racket['#%chido-readtable-postfix-operator], or @racket['#%chido-readtable-notfix-operator].

The second symbol in the result is the operator name, with any leading and trailing underscores stripped but any interior underscores preserved.

Example with inner holes (though the specific example is kind of dumb):
@racketblock[(chido-readtable-add-mixfix-operator 'IF_THEN_ELSE_ my-rt)]
would parse the string
@racket["IF 5 THEN (foobar) ELSE quux"]
as
@racket['(#%chido-readtable-prefix-operator IF_THEN_ELSE 5 (foobar) quux)].

The @racket[layout] argument determines whether layout is required, optional, or disallowed between the operator components and the recursive parses.
Obviously the only good answer here is @racket['required], but if you love poor language design where you can't allow identifiers to include the characters used as operator names you can choose @racket['optional], just know that I think poorly of your choices.
To round things out, you can also choose @racket['none], which I hope everyone can see is just a dumb choice.
But I added it for the sake of completeness.

The @racket[associativity] argument should not need much explanation.
But it should be extended to allow associativity groups, which it currently doesn't support.

The @racket[precedence-greater-than] and @racket[precedence-less-than] allow you to specify relative precedence of operators.
Using these, your operator precedence forms a partial order, and any operators with no ordering between them require disambiguation (IE it's an error to put them together).
(I could have left it ambiguous, but really who wants that?)
I could have written this with numeric precedence instead, but I like partial-order precedence a lot better, so that's what you get in this implementation.
(I mean, it could be extended to have the option of doing either, or maybe both!  But that's more work than I currently want to bother with.)

The symbol pieces of the operator name are blacklisted from the symbol parser.  So if you add the operator @racket['<>=<_>>#>_<$+¢>_] (channeling Haskell), the symbols @racket['<>=<], @racket['>>#>], and @racket['<$+¢>] would be unreadable by the readtable.
This prevents ambiguous parses where you have a parse with the operator as an operator and a parse where you just have the operator name as a symbol in a list of forms.
}

@;@defform[(chido-readtable-add-mixfix-operators [name option ...] ... readtable-expr)]{}


@; TODO - I waffled on whether or not number parsing needed to be built-in, but ultimately decided it doesn't.  So I should remove everything about built-in number support.
@;@defthing[set-chido-readtable-complex-number-support]{
@;Set whether the built-in number parser accepts complex numbers (as default Racket allows).
@;}

@defproc[(set-chido-readtable-symbol-result-transformer [crt chido-readtable?]
                                                        [transformer (-> syntax? any/c)])
         chido-readtable?]{
Parse results from the built-in symbol parsers are syntax objects by default.
By setting a transformer, you can change the result (eg, perhaps you want datums instead of syntax objects).

The default transformer is the identify function.
}

@; TODO - this isn't really supported yet.
@;@defproc[(set-chido-readtable-symbol-literal-delimiters
@;          [crt chido-readtable?]
@;          [l (or/c string? false/c)]
@;          [r (or/c string? false/c)])
@;         chido-readtable?]{
@;Between these delimiters, symbols characters are literal and layout/terminating parsers are ignored.
@;By default both delimiters are pipe characters.
@;If #f is given for the delimiters then no delimiters cause this behavior.
@;}

@;TODO - should I have a procedure for setting or disabling the character that acts like backslash for symbols?

@defproc[(set-chido-readtable-symbol-support [rt chido-readtable?]
                                             [v any/c])
chido-readtable?]{
Turn off or on the built-in symbol parser.
Why would you want to turn it off?
Mostly to re-use the declarative operator precedence and associativity support built into chido-readtables for alternates where you actually don't want a symbol included.

In particular, the BNF DSL is implemented on top of readtables that (generally) have symbol support turned off.
}
@defproc[(chido-readtable-symbol-support? [rt chido-readtable?]) any/c]{
Predicate for whether symbol support is on or off.
}

@defproc[(chido-readtable-blacklist-symbols [symbols (listof (or/c symbol? string?))]
                                            [rt chido-readtable?])
chido-readtable?]{
Produce a readtable where certain symbols are disallowed.
In other words, the built-in symbol parser has a filter on it that checks whether the symbols read are in the blacklist.
}

@defproc[(chido-readtable-name [rt chido-readtable?]) (or/c #f string?)]{
Get the name of the readtable.
}
@defproc[(set-chido-readtable-name [rt chido-readtable?] [name string?])
chido-readtable?]{
Set the name of the chido-readtable (functionally).
}


TODO - other APIs or parsers?

@; TODO - document the readtable-as-dict stuff
@;chido-readtable-dict-ref
@;chido-readtable-dict-set

@; TODO - I should provide some canned readtables -- eg. a racket-equivalent readtable and a recommended default readtable (with cool additions like «» strings)












@section{BNF DSL}

Frankly, the BNF DSL is a little rougher than the rest of the system.
It's more likely to see some minor changes than the rest of the system.

@defform[(define-bnf-arm arm-name top-level-optional ... arm-alt-spec ...)
#:grammar
[(top-level-optional (code:line #:layout layout-flag)
                     (code:line #:layout-parsers layout-parsers)
                     (code:line #:ignore-arm-name? ignore-arm-name?)
                     (code:line #:result/stx result/stx)
                     (code:line #:result/bare result/bare))
 (layout-flag 'required
              'optional
              'none)
 (bnf-arm-alt-spec (code:line binding-sequence-elem ...)
                   (code:line arm-alt-optional ...))
 (arm-alt-optional (code:line #:name name)
                   (code:line #:associativity associativity)
                   (code:line #:precedence-less-than plt)
                   (code:line #:precedence-greater-than pgt)
                   (code:line #:result/stx result/stx)
                   (code:line #:result/bare result/bare))]]{
Construct a @racket[chido-readtable?] that acts like one arm of a BNF grammar.
The result has @racket[chido-readtable-symbol-support?] off.
If @racket[layout-flag] is @racket['required], layout parsers are automatically inserted between parser elements.
If @racket[layout-flag] is @racket['optional], layout-or-epsilon parsers are automatically inserted between parser elements.
The default @racket[layout-flag] is 'optional.

In reality, a parser is always inserted between, but it checks a flag stored on the readtable...

The @racket[layout-parsers] argument should be a list of parsers, and if unspecified defaults to the list @racket['(" " "\t" "\r" "\n")].
Note that these are simply layout parsers on the resulting readtable, so this list can be extended later.

The @racket[result/stx] and @racket[result/bare] arguments act as in the various combinators, but both a global default for the arm and per-alt overrides can be supplied.
If @racket[ignore-arm-name?] is false, the name of the arm is prepended to the result list by the default result constructior.

Each alternative in the “arm” is essentially a @racket[binding-sequence], but with different whole-sequence options.
In particular, if the leftmost or rightmost sequence members are @racket[arm-name], the alternate is determined to be an operator, and the associativity and precedence arguments are used.

TODO - example.
}

@defform[(readtable-extend-as-bnf-arm rt
                                      option ...
                                      bnf-arm-alt-spec ...)
#:grammar
[(option (code:line #:arm-name arm-name)
         (code:line #:ignore-arm-name? ignore-arm-name?)
         (code:line #:result/stx result/stx)
         (code:line #:result/bare result/bare))]]{
Extend @racket[rt] in the manner of @racket[define-bnf-arm].
Each @racket[bnf-arm-alt-spec] is as defined for @racket[define-bnf-arm].

TODO - example.
}

@defform[(define-bnf name
           global-options ...
           [arm-name bnf-arm-option ... bnf-arm-alt-spec ...]
           ...)
#:grammar
[(global-options (code:line #:layout-parsers layout-parsers)
                 (code:line #:layout global-layout-flag))
 (bnf-arm-option (code:line #:main-arm main-arm)
                 (code:line #:layout layout-flag-for-arm)
                 (code:line #:ignore-arm-name? ignore-arm-name)
                 (code:line #:result/stx result/stx)
                 (code:line #:result/bare result/bare))]]{
Define a @racket[bnf-parser?] (which is also a @racket[parser?]) using a BNF grammar.
When the result parser is used directly, the @racket[main-arm] is the parser that is actually used.
The @racket[main-arm] defaults to the top arm.
Note that if you use the parser directly it does NOT parse surrounding layout.
If you want a parser that accepts leading and trailing layout, use @racket[bnf-parser->with-surrounding-layout]?

The @racket[bnf-arm-alt-spec]s you can use in each arm are the same as in @racket[define-bnf-arm].
The layout flag can be set globally but overridden per-arm, with values of @racket['required], @racket['optional], or @racket['none] as per @racket[define-bnf-arm].
The @racket[layout-parsers] again default to @racket['(" " "\t" "\r" "\n")]?

Each arm is a @racket[chido-readtable], as built with @racket[define-bnf-arm], and they can be accessed with @racket[bnf-parser->arm-parser].

TODO - example.
}

@defproc[(bnf-parser? [x any/c]) boolean?]{
Predicate for parsers defined with @racket[define-bnf].
}

@defproc[(bnf-parser->with-surrounding-layout [p bnf-parser?]) parser?]{
Returns a parser like @racket[p] but that allows leading/trailing layout.
}

@defproc[(bnf-parser->arm-parser [p bnf-parser?] [arm-name symbol?])
chido-readtable?]{
Returns the parser for the given arm of the BNF.
It's a @racket[chido-readtable], though probably not one that's necessarily like you might make directly (except through @racket[define-bnf-arm]).
}

TODO - document these

@itemlist[

 @item{extend-bnf -- a form to extend BNFs with an API similar to @racket[define-bnf].}
 @item{define-bnf/quick -- a form to define BNFs like @racket[define-bnf], but using short symbols instead of verbose keyword arguments.}


 @item{define-bnf/syntactic -- a form that defines a BNF using custom concrete syntax that looks more like traditional BNF notation.  It takes a string literal and parses it at macro expansion time.}
 @item{define-bnf/syntactic/parsed -- this actually should be private, I think, but I'm leaving this in the list for the moment...}


 @item{#lang chido-parse/bnf-syntactic -- like @tt{define-bnf/syntactic} but as a #lang.  It allows trailing s-expression definitions and provides the defined bnf as @tt{parser}.}
]
