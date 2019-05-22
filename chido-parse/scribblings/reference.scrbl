#lang scribble/manual

@title{Reference}

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
Chido-readtables are extended with parsers as either @racket['terminating], @racket['soft-terminating], @racket['nonterminating], or @racket['layout] parsers.

When the built-in symbol (or number) parser is used by a chido-readtable, the symbol continues until reaching the prefix of a @racket['terminating] parser or a successful parse from a @racket['soft-terminating] or @racket['layout] parser.

Layout parsers are whitespace parsers that delimit symbols and numbers, and are generally allowed between other parsers inside lists.
Results from layout parsers are ignored when creating parse results from readtable parsers, only the fact that they succeed at parsing is relevant.

Terminating parsers are parsing alternatives that also delimit symbols and numbers (IE they terminate the built-in symbol/number parsing).
In other words no space is necessary between a symbol or number and a following terminating parser.
@racket['soft-terminating] parsers terminate a symbol only of they parse successfully.
Hard terminating (@racket['terminating]) parsers terminate a symbol when their prefix matches, whether or not they parse successfully.
Therefore the prefixes of hard terminating parsers are disallowed within symbols.

Nonterminating parsers are alternatives that do not delimit a symbol or number.
Layout (whitespace) is required between a symbol or number and a following nonterminating parser.

Terminating and nonterminating parsers can follow each other with no layout in between, though layout is allowed between them.

Note that when using a chido-readtable as a parser directly, it parses only ONE form, equivalent to using @racket[chido-readtable->read1-parser].  When you want to parse a sequence using chido-readtables, instead of using a kleene star combinator, you should use @racket[chido-readtable->read*-parser].

Ambiguous results are possible from parsing with a chido-readtable when terminating/nonterminating parsers themselves return ambiguous results or when multiple terminating/nonterminating parsers are successful at a given position.
If another parser is successful, the symbol and number parsers are not tried.
If the symbol and number parsers are used, only one result is returned (a symbol or a number), never an ambiguous result.

@defthing[chido-readtable->read1-parser]{
Turns a @racket[chido-readtable] into an opaque parser that parses a single form (with NO leading whitespace allowed).
The parser is equivalent to using the readtable directly as a parser.

TODO - there should be optional arguments that determine whether it allows leading whitespace and whether it allows a trailing EOF read.
}
@defthing[chido-readtable->read*-parser]{
Turns a @racket[chido-readtable?] into an opaque parser that parses a sequence of forms, returning a list.
This is preferable to using a chido-readtable directly inside a kleene-star combinator because it consistently handles layout parsing and allows trailing layout.
}

@defthing[chido-readtable?]{
Predicate for chido-readtables.
}

@defthing[empty-chido-readtable]{
TODO
-- note that empty-chido-readtable includes no layout!  Not even space!
}

@defthing[racket-chido-readtable]{
TODO -- this should be a readtable analogous to Racket's default readtable.
}

@defthing[clear-chido-readtable]{
(clear-chido-readtable crt prefix-or-parser)

Removes all parsers (terminating, nonterminating, or layout) with the prefix (or the same prefix as) prefix-or-parser.
}

@defthing[extend-chido-readtable]{
TODO --
(extend-chido-readtable crt 'terminating my-foo-parser)

If optional argument #:replace-prefix is true, any parsers with the same prefix as the new parser are removed from the readtable.  (IE it runs clear-chido-readtable before extending.)
}

@defthing[set-chido-readtable-complex-number-support]{
Set whether the built-in number parser accepts complex numbers (as default Racket allows).
}

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
