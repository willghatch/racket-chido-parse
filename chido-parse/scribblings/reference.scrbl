#lang scribble/manual

@title{Reference}

NOTE - chido-parse is NOT YET STABLE, and APIs may change.

@section{Core API}

parse
parse*
whole-parse
whole-parse*

make-parse-derivation
parse-derivation?
parse-derivation-result
parse-derivation-parser
parse-derivation-start-position
parse-derivation-end-position
parse-derivation-derivation-list

parse-failure?
make-parse-failure
TODO - what other failure inspection stuff?

@; TODO - maybe better names?
proc-parser
alt-parser

parser-name
TODO - other parser inspection stuff?

chido-parse-parameter
chido-parse-parameter?
chido-parse-parameterize

parse-stream-cons
for/parse

TODO

@section{Combinators}

TODO
- this should include the expected combinators as well as an improved version of an alternative combinator that includes precidence and associativity filtering.

@section{Filters}

TODO

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
