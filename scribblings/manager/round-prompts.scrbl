#lang scribble/manual

@(require (for-label racket
                     frosthaven-manager/manager))

@title{@tt{manager/round-prompts}}
@defmodule[frosthaven-manager/manager/round-prompts]

This module provides facilities for manipulating round prompt values.

@defform[#:literals (even odd every starting-at)
         (prompt [time rule] ...)
         #:grammar ([rule m-expr
                          even
                          odd
                          (code:line every n-expr starting-at start-expr)])
         #:contracts ([time time/c]
                      [m-expr natural-number/c]
                      [n-expr natural-number/c]
                      [start-expr natural-number/c])]{
Notation for describing round prompt values. The result of a @racket[prompt]
expression is a list of @racket[prompt/c] values.

Each round prompt value is denoted by a @racket[time] and @racket[rule].
@itemlist[
    @item{A rule consisting of @racket[m-expr] means to prompt at round @racket[m-expr].}
    @item{A rule consisting of @racket[even] means to prompt at every even round.}
    @item{A rule consisting of @racket[odd] means to prompt at every odd round.}
    @item{A rule consisting of @racket[every n-expr starting-at start-expr] means to prompt at every @racket[n-expr]th round, starting at the @racket[start-expr]th round.}
]
}

@defproc[(should-do-prompt? [t time/c] [current-round natural-number/c] [prompts (listof prompt/c)])
         any/c]{
Returns true iff at time @racket[t] and round @racket[current-round] the round
prompt rules in @racket[prompts] determine that a prompt should happen.
}

@defthing[prompt/c flat-contract?]{
A serializable round prompt value. A round prompt value expresses rules both for
which round(s) to prompt in and when.
}

@deftogether[(
    @defthing[time/c flat-contract?]
    @defthing[beginning-of time/c]
    @defthing[end-of time/c]
)]{
Times for round prompts representing the beginning of the round and the end of
the round.
}

@defproc[(prompt->string [p prompt/c]) string?]{
A textual description of a round prompt value meant for human use.
}
