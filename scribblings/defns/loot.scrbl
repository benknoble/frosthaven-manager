#lang scribble/manual

@(require (for-label (except-in racket null)
                     frosthaven-manager/defns
                     frosthaven-manager/parsers/formula
                     qi))

@title{Loot Deck}
@defmodule[frosthaven-manager/defns/loot]

@deftogether[(
              @defthing[material-kind? predicate/c]

              @defthing[lumber material-kind?]
              @defthing[metal material-kind?]
              @defthing[hide material-kind?]

              @defthing[material-kinds (listof material-kind?)]
)]{
Represents materials for loot cards.

Serializable.
}

@deftogether[(
              @defthing[herb-kind? predicate/c]

              @defthing[arrowvine herb-kind?]
              @defthing[axenut herb-kind?]
              @defthing[corpsecap herb-kind?]
              @defthing[flamefruit herb-kind?]
              @defthing[rockroot herb-kind?]
              @defthing[snowthistle herb-kind?]

              @defthing[herb-kinds (listof herb-kind?)]
)]{
Represents herbs for loot cards.

Serializable.
}

@deftogether[(
              @defthing[random-item? predicate/c]
              @defthing[random-item random-item?]
)]{
Represents the random-item loot card.

Serializable.
}

@defstruct*[money ([amount natural-number/c])
                  #:transparent]{
Represents a loot card worth 1 to 3 gold, but may have +1 stickers.

Serializable.
}

@defstruct*[material
             ([name material-kind?]
              [amount (apply list/c (build-list (sub1 max-players) (const natural-number/c)))])
             #:transparent]{
Represents a loot card for a material; the amount varies by number of players.
May have +1 stickers.

Serializable.
}

@defproc[(material-amount* [m material?] [n num-players/c]) natural-number/c]{
Calculates the amount a material loot card @racket[m] is worth for the number of
players @racket[n].
}

@defstruct*[herb ([name herb-kind?]
                  [amount natural-number/c])
                 #:transparent]{
Represents a loot card worth 1 @racket[name] herb, but may have +1 stickers.

Serializable.
}

@defthing[loot-card?
           predicate/c
           #:value (or/c money? material? herb? random-item?)]{
This predicate recognizes valid loot cards. It is also a valid
@racket[contract?].
}

@defthing[loot-type/c
           flat-contract?
           #:value (or/c (flow (equal? money)) material-kind? herb-kind? random-item?)]{
This contract recognizes the type of a loot card.
}

@defproc[(card->type [c loot-card?]) loot-type/c]{
Convert a loot card to its type.
}

@defproc[((format-loot-card [n num-players/c]) [card loot-card?]) string?]{
Formats a loot card for display.
}

@deftogether[(
    @defthing[max-money-cards natural-number/c]
    @defthing[max-material-cards natural-number/c]
    @defthing[max-herb-cards natural-number/c]
    @defthing[max-random-item-cards natural-number/c]
)]{
Constants designating the maximum number of certain kinds of cards.
}

@deftogether[(
    @defthing[money-deck (apply list/c (make-list max-money-cards money?))]
    @defthing[material-decks (hash/c material-kind? (apply list/c (make-list max-material-cards material?)))]
    @defthing[herb-decks (hash/c herb-kind? (apply list/c (make-list max-herb-cards herb?)))]
)]{
Decks of loot cards from which you draw to make the loot deck.

Current values are the standard loot cards. Modifications via stickers are not
yet supported.
}

@defproc[(apply-sticker [card (and/c loot-card? (not/c random-item?))])
         loot-card?]{
Returns @racket[card] with amounts increased by @racket[1].
}
