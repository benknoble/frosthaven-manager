#lang scribble/manual

@(require (for-label racket/gui
                     racket/gui/easy))

@title{@tt{gui/helpers}}
@defmodule[frosthaven-manager/gui/helpers]

@defproc[(translate-to-top-coords
           [this (is-a?/c area<%>)]
           [top (is-a?/c area<%>)]
           [x position-integer?]
           [y position-integer?])
         (values position-integer? position-integer?)]{
Returns translated @racket[x] and @racket[y] coordinates relative to
@racket[top], assuming they were originally relative to @racket[this].
}

@defproc[(escape-text [s string?]) string?]{
Escapes @racket[s] for use in @racket[text]; only needed when @racket[s] is
derived from user input.
}
