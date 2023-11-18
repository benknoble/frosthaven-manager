#lang scribble/manual

@(require (for-label racket))

@title{@tt{contracts}}
@defmodule[frosthaven-manager/contracts]

@defproc[(unique-with/c [key (-> any/c any/c)] [c flat-contract?]) contract?]{
The contract @racket[(unique-with/c key c)] requires of a value @racket[_v] that
the result of @racket[(map key _v)] is a @racket[(listof c)] with no duplicates.
}
