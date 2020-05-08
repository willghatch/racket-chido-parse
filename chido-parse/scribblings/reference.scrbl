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
Returns the list of subderivations used in constructing @racket[pd].  Useful for filtering operator precidence and such.
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
Make a @racket[parse-failure].
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


@section{Combinators}

The alternation combinator @racket[alt-parser] is documented in another section.

 sequence
 repetition
 kleene-star
 kleene-plus
 kleene-question
 epsilon-parser
 eof-parser
 not-parser
 peek-parser


 char-parser
 char-range-parser
 any-char-parser
 regexp->parser



 ;; TODO - These are not great, should probably be replaced
 traditional-read-func->parse-result-func
 wrap-derivation
 as-syntax


@section{Filters}

 parse-filter
 follow-filter
 derivation-filter
 result-filter


@section{Chido-Readtable Parsers}

Chido-readtables have an interface similar to Rackets @tech{readtable}s.

Chido-readtables are a fancy kind of alternative parser.
Among their alternatives, chido-readtables have built-in symbol and number parsers whose behavior is modified by the other parsers.
Chido-readtables are extended with parsers and a symbol denoting an effect on the built-in symbol parser, such as @racket['terminating] or @racket['nonterminating-layout].

When the built-in symbol parser is used by a chido-readtable, the symbol continues until reaching the prefix of a @racket['terminating] or @racket['terminating-layout] parser or a successful parse from a @racket['soft-terminating] or @racket['soft-terminating-layout] parser.

Layout parsers are whitespace or comment parsers that delimit symbols and numbers, and are generally allowed between other parsers inside lists.
Results from layout parsers are ignored when creating parse results from readtable parsers, only the fact that they succeed at parsing is relevant.
They come in @racket['terminating-layout], @racket['soft-terminating-layout], and @racket['nonterminating-layout] varieties, which match the symbol affects of @racket['terminating], @racket['soft-terminating], and @racket['nonterminating] parsers, respectively.

Terminating parsers are parsing alternatives that also delimit symbols and numbers (IE they terminate the built-in symbol/number parsing).
In other words no space is necessary between a symbol or number and a following terminating parser.
@racket['soft-terminating] parsers terminate a symbol only of they parse successfully.
Hard terminating (@racket['terminating]) parsers terminate a symbol when their prefix matches, whether or not they parse successfully.
Therefore the prefixes of hard terminating parsers are disallowed anywhere within symbols.

Nonterminating parsers are alternatives that do not delimit a symbol.
Layout (whitespace) is required between a symbol or number and a following nonterminating parser.

Terminating and nonterminating parsers can follow each other with no layout in between, though layout is allowed between them, and usually good style to have.

Note that when using a chido-readtable as a parser directly, it parses only ONE form, equivalent to using @racket[chido-readtable->read1-parser].  When you want to parse a sequence using chido-readtables, instead of using a kleene star combinator, you should use @racket[chido-readtable->read*-parser].

Ambiguous results are possible from parsing with a chido-readtable when terminating/nonterminating parsers themselves return ambiguous results or when multiple terminating/nonterminating parsers are successful at a given position.
If another parser is successful, the symbol parser is not tried.
The symbol parser never returns an ambiguous result.

One final parser flag is @racket['left-recursive-nonterminating].
Parsers with the @racket['left-recursive-nonterminating] flag are run @emph{after} the symbol parser and do not affect symbol parsing.
While chido-parse doesn't normally need a flag to handle left-recursive parsers, because most readtable parsers are run before the symbol parser and used to affect the symbol parser, left-recursive parsers need a special flag in the readtable parser.

@defproc[(chido-readtable->read1-parser [rt chido-readtable?]) parser/c]{
Turns a @racket[chido-readtable] into an opaque parser that parses a single form (with NO leading whitespace allowed).
The parser is equivalent to using the readtable directly as a parser.
}
@defproc[(chido-readtable->read1-parser/layout [rt chido-readtable?]) parser/c]{
Like @racket[chido-readtable->read1-parser], but with leading layout (using the layout parsers within the readtable).
}
@defproc[(chido-readtable->read*-parser [rt chido-readtable?]) parser/c]{
Turns a @racket[chido-readtable?] into an opaque parser that parses a sequence of forms, returning a list.
This is preferable to using a chido-readtable directly inside a kleene-star combinator because it consistently handles layout parsing and allows trailing layout.
The parser allows both leading and trailing layout, and can successfully parse an empty list of forms.

Note that this does NOT parse parentheses at the start/end of a list -- rather, this is a procedure that could be used on the inside of a list.
}
@defproc[(chido-readtable->read+-parser [rt chido-readtable?]) parser/c]{
Like @racket[chido-readtable->read*-parser] but it requires at least one form.
In other words, it fails on empty lists.
}

@defproc[(chido-readtable? [x any/c]) any/c]{
Predicate for chido-readtables.
}

@defthing[empty-chido-readtable]{
A @racket[chido-readtable?] with NO parsers of ANY kind attached, except the built-in symbol parser.


TODO - list the flags that the table starts with, eg. @racket[chido-readtable-symbol-support?].
}


@defproc[(extend-chido-readtable [mode (or/c 'terminating
                                             'soft-terminating
                                             'nonterminating
                                             'terminating-layout
                                             'soft-terminating-layout
                                             'nonterminating-layout
                                             'left-recursive-nonterminating)]
                                 [parser parser/c]
                                 [rt chido-readtable?])
         chido-readtable?]{
TODO -- optional keyword arguments (for operators).

Extend @racket[rt] with @racket[parser] and given @racket[mode].
}

@; TODO - I waffled on whether or not number parsing needed to be built-in, but ultimately decided it doesn't.  So I should remove everything about built-in number support.
@;@defthing[set-chido-readtable-complex-number-support]{
@;Set whether the built-in number parser accepts complex numbers (as default Racket allows).
@;}

@defthing[set-chido-readtable-symbol-result-transformer]{
Parse results from the built-in symbol and number parsers are syntax objects by default.
By setting a transformer, you can change the result (eg, perhaps you want datums instead of syntax objects).

The default transformer is the identify function.
}

@defproc[(set-chido-readtable-symbol-literal-delimiters
[crt chido-readtable?]
[l (or/c string? false/c)]
[r (or/c string? false/c)])
chido-readtable?]{
Between these delimiters, symbols characters are literal and layout/terminating parsers are ignored.
By default both delimiters are pipe characters.
If #f is given for the delimiters then no delimiters cause this behavior.
}

TODO - should I have a procedure for setting or disabling the character that acts like backslash for symbols?

TODO - other APIs?

@section{BNF DSL}

TODO -- I'm not entirely sure what I want here.  Maybe I just want a BNF-like macro among the combinators that lets me specify alt parsers with various built-in filters (precidence, associativity, follow).  Something similar to what other GLL/GLR parser provide, like spoofax's parser and Iguana.  Or maybe I want custom syntax as well -- for a #lang chido-bnf or a chido-bnf macro that parses a string at expansion time.  Either way, I would want the results to be convineintly extensible as well.  (Maybe that means rather than just defining a parser, this will need to define parameters for the parsers of the several nonterminal alternatives that can be extended.)
