#lang scribble/manual

@(require (for-label (except-in racket null)
                     frosthaven-manager/defns
                     frosthaven-manager/manager))

@title{@tt{manager/ability-decks}}
@defmodule[frosthaven-manager/manager/ability-decks]

@defstruct*[ability-decks ([current (or/c #f monster-ability?)]
                           [draw (listof monster-ability?)]
                           [discard (listof monster-ability?)])
                          #:transparent]{
Monster ability deck, with currently active card, draw pile, and discard pile.

Serializable.
}

@defproc[(ability-decks-draw-next [ad ability-decks?]) ability-decks?]{
Draws a card from the ability deck. The value of @racket[(ability-decks-current ad)]
is silently discarded; if it is a @racket[monster-ability?], it is effectively
lost.
}

@defproc[(ability-decks-discard-and-maybe-shuffle [ad ability-decks?])
         ability-decks?]{
Discards the active card and shuffles the ability deck if necessary.
}

@defproc[(update-ability-decks [f (-> string? ability-decks? ability-decks?)])
         (-> (hash/c string? ability-decks?) (hash/c string? ability-decks?))]{
Updates each deck via @racket[f], which is called with the monster set and deck.
}

@defproc[(move-top-draw-to-bottom [ad ability-decks?]) ability-decks?]{
Moves the top card of the draw pile to its bottom.
}
