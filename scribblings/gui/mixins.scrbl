#lang scribble/manual

@(require "../common.rkt")
@(require (for-label racket/gui))

@title{@tt{gui/mixins}}
@defmodule[frosthaven-manager/gui/mixins]

@defproc[(make-closing-proc-mixin [out (-> (-> any) any)])
         (make-mixin-contract top-level-window<%>)]{
Produces a @tech[#:doc ref-doc]{mixin} that calls @racket[out] on instantiation
with a procedure that closes the window. Many uses of @racket[out] are to store
a local binding to this "close" procedure.
}

@defproc[(make-on-close-mixin [proc (-> any)])
         (make-mixin-contract top-level-window<%>)]{
Produces a @tech[#:doc ref-doc]{mixin} that @racket[augment]s @racket[on-close]
to call @racket[proc].
}

@defform[(define-close! close!-id set-close-mixin-id)]{
If the mixin @racket[set-close-mixin-id] is applied to a
@racket[top-level-window<%>] then @racket[close!-id] is a nullary procedure that
closes it.
}

@defmixin[hide-caret/selection (text%) (text%)]{
Augments the text editor to hide the caret but still permit and show selections.
}
