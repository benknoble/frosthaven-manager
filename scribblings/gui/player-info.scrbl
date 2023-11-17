#lang scribble/manual

@(require (for-label (except-in racket null)
                     frosthaven-manager/defns
                     racket/gui/easy
                     racket/gui/easy/contract
                     frosthaven-manager/manager))

@title{@tt{gui/player-info}}
@defmodule[frosthaven-manager/gui/player-info]

@defproc[(player-input-views
           [|@num-players| (obs/c natural-number/c)]
           [#:on-name on-name (-> natural-number/c string? any) void]
           [#:on-hp on-hp (-> natural-number/c (-> number? number?) any) void]
           [#:names names (or/c #f (listof string?)) #f]
           [#:hps hps (or/c #f (listof positive-integer?)) #f])
         (is-a?/c view<%>)]{
A GUI view to enter player names and max HP. The number of entry slots is
determined by @racket[|@num-players|]. The callbacks @racket[on-name] and
@racket[on-hp] are invoked with a player number and a name or a procedure to
modify @racket[player-max-hp]. Default names and max HP values can be specified
via @racket[names] and @racket[hps].
}

@defproc[(player-view
           [|@player| (obs/c player?)]
           [#:on-condition on-condition (-> (list/c condition? boolean?) any) void]
           [#:on-hp on-hp (-> (-> number? number?) any) void]
           [#:on-xp on-xp (-> (-> number? number?) any) void]
           [#:on-initiative on-initiative (-> number? any) void]
           [#:on-summon add-summon (-> string? positive-integer? any)]
           [#:on-summon-hp update-summon-hp (-> natural-number/c (-> number? number?) any)]
           [#:on-summon-condition update-summon-condition (-> natural-number/c (list/c condition? boolean?) any)]
           [#:kill-summon kill-summon (-> natural-number/c any)])
         (is-a?/c view<%>)]{
A GUI view of a single player. See @secref{Player_Controls}. The callback
@racket[on-condition] is given an condition and value that determines whether
the condition should be applied (@racket[#true]) or removed (@racket[#false]).
The callbacks @racket[on-hp] and @racket[on-xp] are given procedures to modify
@racket[player-current-hp] and @racket[player-xp], respectively. The callback
@racket[on-initiative] is given a new initiative for @racket[player-initiative].
The number of players is used to format the player's loot appropriately.

The summon callbacks are given the summon number, a list index, to indicate
which summon to update. Adding a summon is done by name and max HP.
}
