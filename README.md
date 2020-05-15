# Chido Parse

This is a package for the Racket implementation of Parsing With Delimited Continuations.
It allows users to write and compose parsers with any mix of procedural parsers, parsers combinators, BNF, and other parsing abstractions.
It allows parsing procedures to be ambiguous and left-recursive, handling left-recursion by capturing and scheduling delimited continuations.

## Installation
Install with `raco pkg install chido-parse`.

Or, to install the git version, run:

`git clone https://github.com/willghatch/racket-chido-parse chido-parse && cd chido-parse && raco pkg install`

or

`raco pkg install --clone chido-parse`

## Documentation

Documentation is [online](http://docs.racket-lang.org/chido-parse/index.html), or
available by running `raco docs chido-parse` after installing.
