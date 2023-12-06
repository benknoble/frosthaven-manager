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
              @defthing[to-start transition/c]
              @defthing[to-input-player-info transition/c]
              @defthing[to-build-loot-deck transition/c]
              @defthing[to-add-prompts transition/c]
              @defthing[to-choose-monster-db transition/c]
              @defthing[to-choose-monsters-or-play transition/c]
              @defthing[to-choose-monsters transition/c]
              @defthing[to-play transition/c]
              @defthing[next-round transition/c]
              @defthing[draw-abilities transition/c]
              )]{
A series of transition functions.
}
