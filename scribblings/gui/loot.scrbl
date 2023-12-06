#lang scribble/manual

@(require (for-label (except-in racket null)
                     frosthaven-manager/defns
                     racket/gui/easy
                     racket/gui/easy/contract
                     frosthaven-manager/gui/loot))

@title{@tt{gui/loot}}
@defmodule[frosthaven-manager/gui/loot]

@defproc[(loot-picker
           [|@|cards-per-deck (obs/c (hash/c (listof loot-card?) natural-number/c))]
           [|@|stickers-per-deck (obs/c (hash/c (listof loot-card?) natural-number/c))]
           [#:on-card on-card (-> (list/c (or/c 'add 'remove) (listof loot-card?)) any) void]
           [#:on-sticker on-sticker (-> (list/c (or/c 'add 'remove) (listof loot-card?)) any) void])
         (is-a?/c view<%>)]{
A GUI view to build a loot deck by including certain loot cards. The callback
@racket[on-card] is invoked with an "event" that specifies a deck of cards from
which one card should be added or removed. Similarly for @racket[on-sticker] to
add stickers to decks.
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

See @secref{Scenario_Information_and_Loot} for how @racket[loot-button]
functions in Frosthaven Manager.
}

@defproc[(loot-preview [|@loot-deck| (obs/c (listof loot-card?))]
                       [|@num-players| (obs/c num-players/c)])
         (is-a?/c view<%>)]{
A button that, when clicked, shows a loot deck previewer.
}
