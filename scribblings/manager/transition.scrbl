#lang scribble/manual

@(require (for-label racket
                     racket/gui/easy
                     frosthaven-manager/manager))

@title{@tt{manager/transition}}
@defmodule[frosthaven-manager/manager/transition]

@defthing[transition/c contract? #:value (-> state? (-> any))]{
A transition function consumes a @racket[state?] and produces a thunk typically
used as the action for a @racket[button].
}

@deftogether[(
              @defthing[next-round transition/c]
              @defthing[draw-abilities transition/c]
              )]{
Transition functions that define how to progress through the round structure of
the game. These are idempotent in that if they are called with the wrong
@racket[state-@in-draw?], they do nothing.
}
