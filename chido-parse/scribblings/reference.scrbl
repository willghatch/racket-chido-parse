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
Chido-readtables are extended with parsers as either @racket['terminating], @racket['nonterminating], or @racket['layout] parsers.

Layout parsers are whitespace parsers that delimit symbols and numbers.
Results from layout parsers are generally ignored.
Two symbols or numbers in sequence require a layout parser between them.

Terminating parsers are parsing alternatives that also delimit symbols and numbers (IE they terminate the built-in symbol/number parsing).
In other words no space is necessary between a symbol or number and a following terminating parser.

Nonterminating parsers are alternatives that do not delimit a symbol or number.
A space is required between a symbol or number and a following nonterminating parser.

Terminating and nonterminating parsers can follow each other with no layout in between.

Note that when using a chido-readtable as a parser directly, it parses only ONE form, equivalent to using @racket[chido-readtable->read1-parser].  When you want to parse a sequence using chido-readtables, instead of using a kleene star combinator, you should use @racket[chido-readtable->read*-parser].

@defthing[chido-readtable->read1-parser]{
Turns a @racket[chido-readtable] into an opaque parser that parses a single form (with leading whitespace allowed).
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

@defthing[extend-chido-readtable]{
TODO --
(extend-chido-readtable crt 'terminating my-foo-parser)
}

TODO - other APIs?

@section{BNF DSL}

TODO
