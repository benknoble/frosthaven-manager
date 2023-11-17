#lang scribble/manual

@(require (for-label racket
                     pict
                     frosthaven-manager/aoe-images))

@title{@tt{aoe-images}}
@defmodule[frosthaven-manager/aoe-images]

This module provides procedures for constructing area-of-effect diagrams.

@defparam[hex-size size natural-number/c #:value 30]{
The size of the hexes built from this module.
}

@defproc[(r) (and/c positive? number?)]{
Returns the amount by which odd rows need shifted to align with even rows in a
hex-grid (based on @racket[hex-size]).
}

@deftogether[(
              @defproc[(S) pict?]
              @defproc[(X) pict?]
              @defproc[(O) pict?]
              @defproc[(M) pict?]
)]{
Hexes for an area-of-effect diagram: respectively, spacers, attacks, allies, and
the initiating figure.
}

@defproc[(border-size [max-row natural-number/c] [max-col natural-number/c])
         (and/c positive? number?)]{
Returns the side-length of a square rectangle which would encompass an
area-of-effect diagram of @racket[max-row] rows and @racket[max-col] columns in
a hex-grid, if the diagram were centered and superimposed on the rectangle à la
@racket[cc-superimpose].
}

@deftogether[(
              @defthing[spec-sym? flat-contract? #:value (or/c 's 'x 'o 'm 'g)]
              @defthing[spec?
                         flat-contract?
                         #:value
                         (listof (list/c exact-positive-integer?
                                         boolean?
                                         (listof (list/c spec-sym? natural-number/c))))]
              @defproc[(spec->shape [s spec?]) pict?]
)]{
Convert an AoE spec to a shape. The spec contains a list of rows; each row
contains a line number, a flag indicating this line should be offset relative
to the lines above and below it (which are not necessarily in the spec), and a
list of column specifiers, pairing symbols with columns in sorted order.

The symbols represent the corresponding shapes, with @racket['g] a
@racket[ghost] hex.
}

@deftogether[(
              @defthing[syntaxes-can-be-spec? predicate/c]
              @defproc[(syntaxes->spec [stxs (and/c (listof syntax?) syntaxes-can-be-spec?)])
                       spec?]
)]{
Convert a list of syntax objects to a @racket[spec?].
}

@defproc[(string->spec [s string?]) spec?]{
Uses @racket[syntaxes->spec] on syntax read from @racket[s] by
@racket[read-syntax] to produce an AoE spec. Fails if the resulting syntaxes
cannot be a spec, as defined by @racket[syntaxes-can-be-spec?].
}
