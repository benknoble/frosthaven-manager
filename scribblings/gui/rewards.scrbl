#lang scribble/manual

@(require (for-label (except-in racket/gui null)
                     frosthaven-manager/defns
                     racket/gui/easy
                     racket/gui/easy/contract))

@title{@tt{gui/rewards}}
@defmodule[frosthaven-manager/gui/rewards]

This module contains views for end-of-scenario rewards.

@defproc[(player-rewards-view [|@|num-players (obs/c num-players/c)]
                              [|@|level (obs/c level/c)]
                              [|@|players (obs/c (listof player?))]
                              [#:mixin mix (make-mixin-contract top-level-window<%>) values])
         (is-a?/c view<%>)]{
Produces a @racket[window] for displaying a players rewards, such as loot, gold,
and XP. The selected player's loot cards are also displayed. The mixin
@racket[mix] is applied to the @racket[window].
}
