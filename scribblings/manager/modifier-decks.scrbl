#lang scribble/manual

@(require (for-label (except-in racket null)
                     racket/gui/easy
                     frosthaven-manager/defns
                     frosthaven-manager/manager
                     frosthaven-manager/gui/monster-modifier))

@title{@tt{manager/modifier-decks}}
@defmodule[frosthaven-manager/manager/modifier-decks]

This module provides facilities for manipulating the modifier deck.

@defproc[(reshuffle-modifier-deck [s state?]) any]{
Reshuffle the monster modifier deck.
}

@defproc[(discard [s state?] [card monster-modifier?]) any]{
Discard a card to the appropriate pile.
}

@defproc[(draw-modifier [s state?]) (-> any)]{
Draws and discards a single modifier card.
}

@defproc[(draw-modifier* [s state?] [keep (-> monster-modifier? monster-modifier? monster-modifier?)])
         (-> any)]{
Draws two modifier cards and discards them with the kept card on top.
}

@deftogether[(@defproc[(do-curse-monster [s state?]) (-> any)]
              @defproc[(do-bless-monster [s state?]) (-> any)]
              @defproc[(do-bless-player [s state?]) (-> any)]
              @defproc[(do-unbless-player [s state?]) (-> any)])]{
Add a curse or bless to the appropriate deck.
}

@deftogether[(@defproc[(add-monster-modifier [s state?]) (-> monster-modifier? any)]
              @defproc[(remove-monster-modifier [s state?]) (-> exact-nonnegative-integer? any)])]{
Handles events emitted by @racket[card-swapper]. These are intended for use when
the deck handed to @racket[card-swapper] consists of something like this:
@racketblock[
    (obs-combine append (state-@monster-modifier-deck s) (state-@monster-discard s))
]
In particular, @racket[remove-monster-modifier] translates indexes to both the
modifier deck and the discard pile.
}
