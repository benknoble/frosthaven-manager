#lang scribble/manual

@(require (for-label racket
                     megaparsack
                     frosthaven-manager/parsers/formula))

@title{@tt{parsers/formula}}
@defmodule[frosthaven-manager/parsers/formula]

This module contains parsers for arithmetic formulas over addition, subtraction,
multiplication, division, rounding, and a limited set of variables. The parse
result is a function from an environment of variables to a number.

@deftogether[(
              @defthing[env/c flat-contract? #:value (hash/c (or/c "L" "C") number? #:flat? #t)]
              @defthing[expr/pc contract? #:value (-> env/c number?)]
)]{
Contracts for the parse results of formulas.
}

@defthing[expr/p (parser/c char? expr/pc)]{
Textual parser for formulas.
}

@defproc[(parse-expr [in string?]) expr/pc]{
Parses a string as a formula or fails.
}
