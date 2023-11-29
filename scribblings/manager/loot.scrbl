#lang scribble/manual

@(require (for-label (except-in racket null)
                     frosthaven-manager/defns
                     frosthaven-manager/manager
                     frosthaven-manager/gui/loot))

@title{@tt{manager/loot}}
@defmodule[frosthaven-manager/manager/loot]

This module provides facilities for manipulating the loot deck.

@defproc[(update-loot-deck-and-num-loot-cards [s state?])
         (-> (list/c (or/c 'add 'remove) (listof loot-card?)) any)]{
Update the loot deck based on the @racket[loot-picker] event.
}

@defproc[((update-stickers-per-deck [s state?])
          [evt (list/c (or/c 'add 'remove) (listof loot-card?))])
         any]{
Updates @racket[(state-|@stickers-per-loot-deck| s)] based on the event
@racket[evt] as described in @racket[loot-picker] by updating the count of
stickers per deck.
}

@defproc[(build-loot-deck [cards-per-loot-deck (hash/c (listof loot-card?) natural-number/c)]
                          [stickers-per-loot-deck (hash/c (listof loot-card?) natural-number/c)])
         (listof loot-card?)]{
Converts a count of cards per deck into an shuffled deck of loot cards. This can
be considered the interpreter for a language whose values are like those
produced by @racket[loot-picker] and @racket[update-stickers-per-deck]; namely,
mappings from decks to number of cards.
}

@defproc[(build-loot-deck! [s state?]) any]{
Updates @racket[s] by applying @racket[build-loot-deck].
}

@defproc[((give-player-loot [s state?]) [k any/c]) any]{
Give player @racket[k] the top loot card.
}

@defproc[(place-loot-on-bottom [s state?]) any]{
Rotate the top loot card to the bottom of the deck.
}

@defproc[(player->rewards [p player?] [num-players num-players/c] [level level/c])
         (listof string?)]{
Each string is a reward for player @racket[p] except the first, which is the
player's name. The values are an indicator if the player got the random item,
the player's XP, gold, each material amount, and each herb amount.
}
