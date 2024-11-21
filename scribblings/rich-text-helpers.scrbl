#lang scribble/manual

@(require (for-label racket
                     frosthaven-manager/rich-text-helpers)
          scribble/example)

@(define my-eval
   (make-base-eval '(require frosthaven-manager/rich-text-helpers)))
@(define-syntax-rule (submod-link mod sub ...)
   @racketmodlink[(submod mod sub ...)]{@racket[(submod mod sub ...)]})

@title{@tt{rich-text-helpers}}
@defmodule[frosthaven-manager/rich-text-helpers]

This module provides helpers for converting values to the rich text model of
@submod-link[frosthaven-manager/gui/rich-text-display model]. In particular,
these helpers work with functions that take single values to lists which are
intended to be spliced into the resulting model list.

@defproc[(only-on-text [f (-> any/c ... list?)] [arg any/c] ...)
         (-> any/c list?)]{
Returns a function over @racket[_x] that returns @racket[(f arg ... _x)] when
@racket[_x] is a string, or @racket[(list _x)] otherwise. Particularly useful
with @racket[append-map] over a model value and a function @racket[f] that can
only handle strings.
}

@defform[(match-loop input-expr
           [match-pattern body-expr ... result-expr] ...)
         #:contracts ([result-expr list?])]{
Iterates like a fixed-point computation. First, match @racket[input-expr]
against the @racket[match-pattern] and evalute the body and result expressions,
like @racket[match]. If there is no match, the return @racket[(list input-expr)].
If there is a match, @racket[append-map] the same @racket[match-loop]
computation over each of the elements of the result.

@examples[#:eval my-eval
  (define (f x)
    (match-loop x
      [(regexp #rx"^(.*)body(.*)$" (list _ prefix suffix))
       (list prefix 42 suffix)]))
  (f "begin; body 1; body 2; body 3; end")
]

A major difference from something like @racket[regexp-replace*] or
@racket[regexp-replaces] is that the inputs and replacements can be arbitrary
values; this codifies the "replace all" loop. Many uses include the same kind of
prefix/suffix matching seen in the example above, but the algorithm is more
general and can find newly generated matches in @racket[result-expr]. Take care
to avoid generating an infinite loop by unconditionally placing a new match in
the result.
}

@(close-eval my-eval)
