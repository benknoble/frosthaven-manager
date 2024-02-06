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

@defproc[(build-loot-deck
           [type->number-of-cards (hash/c loot-type/c natural-number/c)]
           [type->deck (hash/c loot-type/c (listof loot-card?))])
         (listof loot-card?)]{
Converts two mappings into a shuffled deck of loot cards. This can be considered
the interpreter for a language whose values are like those produced by
@racket[loot-picker]; namely, mappings from decks to number of cards.

The mapping @racket[type->number-of-cards] maps a loot card type to a number of
loot cards of that type. The mapping @racket[type->deck] specifies which deck
cards of that type should be drawn from.

This function assumes, but does not check, that for all types @racket[_t] the
number of cards for that type @racket[(hash-ref type->number-of-cards _t)] is
less than or equal to the number of loot cards in that type's deck
@racket[(length (hash-ref type->deck _t))].
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
