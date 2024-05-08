#lang scribble/manual

@(require (for-label (except-in racket null)
                     frosthaven-manager/defns
                     racket/gui/easy
                     racket/gui/easy/contract
                     frosthaven-manager/manager
                     frosthaven-manager/gui/monster-modifier))

@title{@tt{gui/monster-modifier}}
@defmodule[frosthaven-manager/gui/monster-modifier]

@defproc[(modify-monster-deck-menu-item [|@|cards (obs/c (listof monster-modifier?))]
                                        [#:on-add on-add (-> monster-modifier? any) void]
                                        [#:on-remove on-remove (-> exact-nonnegative-integer? any) void]
                                        [#:on-shuffle on-shuffle (-> any)])
         (is-a?/c view<%>)]{
A menu-item rendering @racket[favors-dialog].
}

@defproc[(favors-dialog [|@|cards (obs/c (listof monster-modifier?))]
                        [#:on-add on-add (-> monster-modifier? any) void]
                        [#:on-remove on-remove (-> exact-nonnegative-integer? any) void]
                        [#:on-shuffle on-shuffle (-> any)])
         (is-a?/c window-view<%>)]{
A dialog that wraps @racket[card-swapper] and adds a @onscreen{Shuffle} button
which emits @racket[on-shuffle].
}

@defproc[(card-swapper [|@|cards (obs/c (listof monster-modifier?))]
                       [#:on-add on-add (-> monster-modifier? any) void]
                       [#:on-remove on-remove (-> exact-nonnegative-integer? any) void])
         (is-a?/c view<%>)]{
A view that permits swapping cards in or out of @racket[|@|cards], a subset of
@racket[monster-modifier-deck] according to @racket[absent-from-modifier-deck],
by clicking @onscreen{<=} and @onscreen{=>} arrows.

When a card is added via the @onscreen{<=} arrow, calls @racket[on-add] with the
card. When a card is removed via the @onscreen{=>} arrow, calls @racket[on-remove]
with the cards index.
}
