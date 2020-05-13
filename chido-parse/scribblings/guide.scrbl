#lang scribble/manual

@(require (for-label racket/base chido-parse))

@title{Guide}

NOTE - chido-parse is NOT YET STABLE, and APIs may change.


Chido Parse is an expressive, extensible, composable parsing system.
It allows users to build and compose parsers using any mix of ad-hoc procedural parsing, parser combinators, and other parsing abstractions and interfaces, such as readtables and BNF.
It allows ambiguous grammars, but provides tools to filter ambiguity, including declarative operator associativity and precidence rules in readtables and BNF rules.
Chido Parse supports context-sensitive grammars, including data-dependent grammars, in addition to supporting the entire class of context-free grammars.

Chido Parse is also... slow.
So until and unless it can be optimized about 1 order of magnitude faster, I don't expect people to really use it except for quick prototyping.
If it gets 1 order of magnitude faster it will still be slow, but probably faster than macro expansion (at least for s-expression based languages), which will make it viable for projects where extensible and composable concrete syntax extension is as important as macro-based extension.
But if you want fast compilation... look elsewhere.

You can use Chido Parse by constructing parser objects and then using @racket[whole-parse] to parse your input.
The simplest kind of parser to use is a literal string, and you can easily use combinators like @racket[sequence].

@racket[(define abc-d (whole-parse "abc" (sequence "a" "b" "c")))]

The above code will return a derivation object.  We can get a semantic result out with @racket[parse-derivation-result]:

@racket[(parse-derivation-result abc-d)]

Which will return @racket['("a" "b" "c")].

One of the key features of Chido Parse is its support for procedural parsers.
Parsing procedures take a port and return either a derivation or a parse failure.
To use them as parsers, you need to wrap them with the @racket[proc-parser] struct.
We can easily re-use existing parsers this way.

@racket[(proc-parser
         (λ (port) (make-parse-derivation
                    (read-syntax (object-name port) port))))]

Actually, procedural parsers can return a @racket[stream] tree of parse derivations and failures.
This allows them to support ambiguous grammars.
However, result streams for Chido Parse should be constructed with @racket[stream-cons] or @racket[for/parse], unless you know what you're doing.

Procedural parsers can recur back into the parsing system by using @racket[parse*].
@racket[parse*] is like @racket[whole-parse], except that it doesn't have to consume the whole input and it returns a stream of derivations.
Note that @racket[whole-parse] actually returns a parse failure if there is more than one derivation.
If you want multiple results from a top-level parse, you can use @racket[whole-parse*].
While parsers can return a tree of streams, they are flattened and @racket[parse*] and @racket[whole-parse*] return a stream whose members are all @racket[parse-derivation?] objects.

For example:

@racketblock[
(define plus
  (proc-parser
   (λ (port)
     (for/parse ([l (parse* port expression)])
       (for/parse ([op (parse* port "+" #:start l)])
         (for/parse ([r (parse* port expression #:start op)])
                    (make-parse-derivation
                     `(+ ,(parse-derivation-result l)
                         ,(parse-derivation-result r))
                     #:derivations (list l op r))))))))
]

Note that @racket[parse*] does not actually consume input from the port, so successive parses need the @racket[#:start] keyword.
But there's an easier way.
Instead of @racket[parse*], you can use @racket[parse*-direct], which uses delimited continuation magic to loop over a stream while looking like straight-line code:

@racketblock[
(define plus
  (proc-parser
   (λ (port)
     (define l (parse*-direct port expression))
     (define op (parse*-direct port "+"))
     (define r (parse*-direct port expression))
     (make-parse-derivation
      `(+ ,(parse-derivation-result l)
          ,(parse-derivation-result r))
      #:derivations (list l op r)))))
]

Note that if we define the expression parser like so:

@racketblock[
(define expression
  (alt-parser plus number))
(define number
  (code:comment "4 is a perfectly acceptable number.")
  "4")
]

It turns out that our procedural plus parser is left-recursive!
Everybody knows that if you make a left-recursive procedural parser you get infinite recursion!
But not in Chido Parse.
Chido Parse dynamically detects left-recursive dependency cycles and resolves them by capturing and descheduling first-class delimited continuations.
When a cycle occurs, Chido Parse saves the continuation, finds other work, and then applies the continuation to a result when one is available from a different alternate.
If there is no alternate that can make progress, Chido Parse breaks the cycle by returning a failure result to the left-recursive parser.
Failing cycles this way is mostly so the parser can compute a good failure message, but conceivably you could implement a biased alternate this way.
However, at present Chido Parse doesn't guarantee any order to cycle breaking, so don't.

Chido Parse also has BNF forms, of course:

@racketblock[
(define-bnf dumb-statement-parser
    [statement ["pass"]
               ["for" id "in" expression "do" statement]
               ["{" [: #:repeat-min 0 #:splice 1 statement] "}" #:name "block"]
               [expression]]
    [expression [number-parser]
                [id]
                [expression "+" expression #:associativity 'left]
                [expression "*" expression
                            #:associativity 'right
                            #:precedence-greater-than '("+")]]
    (code:comment "Obviously this is a bad identifier parser, but I'm making dumb examples in a hurry.")
    [id [[: #:repeat-min 1 (char-range "az")]]])
]

The @racket[define-bnf] form has a bunch of optional arguments for specifying automatic layout insertion (IE allowing whitespace between things).
Note that these things are all just parsers.
You can use a procedural parser anywhere in your BNF definition, you can stick a BNF parser in a combinator, etc.

Of course, the real reason I started Chido Parse was because I wanted better readtables.
So Chido Parse has a readtable-style abstraction for making and extending fancy s-expression parsers.

@racketblock[
(define cool-s-exp
  (chido-readtable-add-list-parser "$(" ")" basic-chido-readtable
                                   #:wrap '#%dollar-paren))
]

Now @tt{cool-s-exp} can parse

@verbatim|{
(foo bar $(quux flub) zonkers)
}|

as @racket['(foo bar (#%dollar-paren quux flub) zonkers)]

Chido readtables support infix/prefix/postfix operators as well as the kinds of extensions that Racket's normal readtables support.
Chido readtables support arbitrary-length prefixes unlike Racket readtables with their single character prefix..

Chido readtables and BNF parsers can be extended, eg. with @racket[extend-chido-readtable] and @racket[extend-bnf].
BNF parsers are actually made out of readtables, and you can do @racket[extend-readtable-as-bnf].

Parsers can be extended, modified, or loaded dynamically.
Eg. you can use @racket[extend-readtable] during parsing, and you can create parsers like Racket's built-in @tt{#reader} form.
All these fancy parsing abstractions are built out of procedural parsers.
If there is another parsing abstraction you know of and like, you can build that abstraction too!

This is clearly a bad guide, but I wanted to write at least something real quick.

TODO - write a better guide.


