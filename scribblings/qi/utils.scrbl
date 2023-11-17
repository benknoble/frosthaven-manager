#lang scribble/manual

@(require (for-label racket
                     qi
                     frosthaven-manager/qi/utils))

@title{@tt{qi/utils}}
@defmodule[frosthaven-manager/qi/utils]

@defproc[(list-remove [xs list?] [i natural-number/c])
         (values list? any/c)]{
Returns a new list consisting of all elements of @racket[xs] excepth the
@racket[i]th, and the @racket[i]th value. If @racket[i] is out of bounds, raises
an exception.
}
