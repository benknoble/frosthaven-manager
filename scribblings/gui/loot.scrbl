#lang scribble/manual

@(require (for-label (except-in racket null)
                     frosthaven-manager/defns
                     racket/gui/easy
                     racket/gui/easy/contract
                     frosthaven-manager/gui/loot
                     frosthaven-manager/manager/loot))

@title{@tt{gui/loot}}
@defmodule[frosthaven-manager/gui/loot]

@defproc[(loot-picker
           [|@|type->cards (obs/c (hash/c loot-type/c natural-number/c))]
           [|@|type->deck (obs/c (hash/c loot-type/c (listof loot-card?)))]
           [#:on-card on-card (-> (list/c (or/c 'add 'remove) loot-type/c) any) void]
           [#:on-deck on-deck (-> (hash/c loot-type/c (listof loot-card?)) any) void])
         (is-a?/c view<%>)]{
A GUI view to build a loot deck by including certain loot cards. The callback
@racket[on-card] is invoked with an "event" that specifies a type of cards from
which one card should be added or removed. The callback @racket[on-deck] is
invoked with a mapping from loot types to decks that should be used to interpret
what decks to draw cards from. See @racket[build-loot-deck].

This picker allows loading new decks of loot cards and stickering cards on the
fly with @onscreen{+ 1} stickers. It does not allow removing such stickers;
reset the deck to start over.
}

@defproc[(loot-button
           [|@loot-deck| (obs/c (listof loot-card?))]
           [|@num-loot-cards| (obs/c natural-number/c)]
           [|@num-players| (obs/c num-players/c)]
           [|@players| (obs/c (listof (cons/c player? any/c)))]
           [#:on-player on-player (-> any/c any) void]
           [#:on-top on-top (-> any) void]
           [#:on-bottom on-bottom (-> any) void])
         (is-a?/c view<%>)]{
A GUI view of a button that, when clicked, shows a view to assign the top loot
card from @racket[|@loot-deck|] to one of @racket[|@players|] via buttons. The
callback @racket[on-player] is invoked with the ID (@racket[cdr]) of the player
from @racket[|@players|] whose button is clicked to assign loot; it can be used
to, @italic{e.g.}, assign the loot card. After @racket[on-player] is invoked,
the view is closed.

Additionally, buttons for the top and bottom of the deck trigger the
@racket[on-top] and @racket[on-bottom] callbacks, which then also close the
view.

See @secref{Scenario_information_and_loot} for how @racket[loot-button]
functions in Frosthaven Manager.
}

@defproc[(loot-preview [|@loot-deck| (obs/c (listof loot-card?))]
                       [|@num-players| (obs/c num-players/c)])
         (is-a?/c view<%>)]{
A button that, when clicked, shows a loot deck previewer.
}
