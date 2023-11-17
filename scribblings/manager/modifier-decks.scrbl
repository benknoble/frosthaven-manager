#lang scribble/manual

@(require (for-label (except-in racket null)
                     frosthaven-manager/defns
                     frosthaven-manager/manager))

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
