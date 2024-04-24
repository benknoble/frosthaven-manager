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

@defform[(define-error-text |@error-text-id| with-error-text-id)]{
Binds @racket[|@error-text-id|] to an observable string and
@racket[with-error-text-id] to a form accepting arbitrarily many expressions.
The form resets @racket[|@error-text-id|] evaluates all of its body expressions
and returns the result of the last one; if any raise an exception, instead, the
exception's error message is stored in @racket[|@error-text-id|] and returned
from the form.
}
