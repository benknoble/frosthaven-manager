#lang scribble/manual

@(require (for-label (except-in racket null)
                     frosthaven-manager/defns
                     frosthaven-manager/parsers/formula))

@title{Players}
@defmodule[frosthaven-manager/defns/players]

@defstruct*[player
             ([name string?]
              [max-hp positive-integer?]
              [current-hp natural-number/c]
              [xp natural-number/c]
              [conditions (listof condition?)]
              [initiative initiative?]
              [loot (listof loot-card?)]
              [summons (listof summon?)])
             #:transparent]{
A @racket[player] captures everything about a player that Frosthaven Manager
needs.

You will not usually need the @racket[player] constructor: use the smart
constructor @racket[make-player] instead.

Serializable.
}

@defproc[(make-player [name string?] [max-hp positive-integer?]) player?]{
Creates a @racket[player] with @racket[name] and @racket[max-hp].
}

@defproc[((player-update-name [new-name string?])
          [p player?])
         player?]{
Transforms @racket[p] to have @racket[player-name] equal to @racket[new-name].
}

@defproc[((player-act-on-hp [f (-> natural-number/c number?)])
          [p player?])
         player?]{
Transforms @racket[(player-current-hp p)] by @racket[f]. If the result is not
@racket[positive?] the update is ignored.
}

@defproc[((player-act-on-max-hp [f (-> natural-number/c number?)])
          [p player?])
         player?]{
Similarly to @racket[player-act-on-hp], transforms @racket[(player-max-hp p)] by
@racket[f]. If the result is not @racket[positive?] the update is ignored.
}

@defproc[((player-act-on-xp [f (-> natural-number/c number?)])
          [p player?])
         player?]{
Similarly to @racket[player-act-on-hp], transforms @racket[(player-xp p)] by
@racket[f]. If the result is not @racket[natural?] the update is ignored.
}

@deftogether[(
              @defproc[((player-add-condition [c condition?]) [p player?]) player?]
              @defproc[((player-remove-condition [c condition?]) [p player?]) player?]
)]{
Transforms @racket[(player-conditions p)] by adding or removing condition
@racket[c].
}

@defproc[((player-condition-handler [c? (list/c condition? boolean?)])
          [p player?])
          player?]{
Dispatches to @racket[player-add-condition] or @racket[player-remove-condition]
based on @racket[(second c?)]: @racket[#true] means
@racket[player-add-condition]. The condition to be added or removed is
@racket[(first c?)].
}

@defproc[((player-afflicted-by? [c condition?])
          [p player?])
         boolean?]{
True if-and-only-if @racket[(player-conditions c)] includes @racket[c].
}

@defproc[(player-expire-conditions [p player?]) player?]{
Remove @racket[expirable-conditions] from @racket[p].
}

@defproc[(player-dead? [p player?]) boolean?]{
True if-and-only-if @racket[(player-current-hp p)] is @racket[zero?].

In practice, player HP does not currently fall below 1. This may be a bug.
}

@defproc[(player-at-max-health? [p player?]) boolean?]{
True if-and-only-if @racket[(player-current-hp p)] is @racket[(player-max-hp p)].
}

@deftogether[(
              @defproc[(player-set-initiative [p player?] [i initiative?]) player?]
              @defproc[(player-clear-initiative [p player?]) player?]
)]{
Transforms @racket[(player-initiative p)] by setting it to @racket[i] or
clearing it to @racket[0].
}

@defproc[((player-add-loot [card loot-card?])
          [p player?])
          player?]{
Transforms @racket[(player-loot p)] by adding @racket[card].
}

@defproc[(player->hp-text [p player?]) string?]{
Formats the string @racket["HP: current/max"] for the player @racket[p].
}

@defproc[(player-conditions* [p player?]) (listof condition?)]{
Same as @racket[(player-conditions p)] but sorted.
}

@defstruct*[summon
             ([name string?]
              [max-hp positive-integer?]
              [current-hp natural-number/c]
              [conditions (listof condition?)])
             #:transparent]{
A player summon. Serializable.
}

@defproc[((summon-update-name [new-name string?])
          [s summon?])
         summon?]{
Transforms @racket[s] to have @racket[summon-name] equal to @racket[new-name].
}

@defproc[((summon-act-on-hp [f (-> natural-number/c number?)])
          [s summon?])
         summon?]{
Transforms @racket[(summon-current-hp s)] by @racket[f]. If the result is not
@racket[positive?] the update is ignored.
}

@defproc[((summon-act-on-max-hp [f (-> natural-number/c number?)])
          [s summon?])
         summon?]{
Similarly to @racket[summon-act-on-hp], transforms @racket[(summon-max-hp s)] by
@racket[f]. If the result is not @racket[positive?] the update is ignored.
}

@deftogether[(
              @defproc[((summon-add-condition [c condition?]) [s summon?]) summon?]
              @defproc[((summon-remove-condition [c condition?]) [s summon?]) summon?]
)]{
Transforms @racket[(summon-conditions s)] by adding or removing condition
@racket[c].
}

@defproc[((summon-condition-handler [c? (list/c condition? boolean?)])
          [s summon?])
          summon?]{
Dispatches to @racket[summon-add-condition] or @racket[summon-remove-condition]
based on @racket[(second c?)]: @racket[#true] means
@racket[summon-add-condition]. The condition to be added or removed is
@racket[(first c?)].
}

@defproc[((summon-afflicted-by? [c condition?])
          [s summon?])
         boolean?]{
True if-and-only-if @racket[(summon-conditions c)] includes @racket[c].
}

@defproc[(summon-dead? [s summon?]) boolean?]{
True if-and-only-if @racket[(summon-current-hp s)] is @racket[zero?].

In practice, summon HP does not currently fall below 1. This may be a bug.
}

@defproc[(summon-at-max-health? [s summon?]) boolean?]{
True if-and-only-if @racket[(summon-current-hp s)] is @racket[(summon-max-hp s)].
}

@defproc[(summon->hp-text [s summon?]) string?]{
Formats the string @racket["HP: current/max"] for the summon @racket[s].
}

@defproc[(summon-conditions* [s summon?]) (listof condition?)]{
Same as @racket[(summon-conditions s)] but sorted.
}

@defproc[(player-summon [p player?] [name string?] [max-hp positive-integer?])
         player?]{
Adds a @racket[summon] to @racket[p].
}

@defproc[((update-player-summon [i natural-number/c] [f (-> summon? summon?)])
          [p player?])
         player?]{
Update the @racket[i]th @racket[summon] via @racket[f].
}

@defproc[((player-kill-summon [i natural-number/c]) [p player?]) player?]{
Kill the @racket[i]th @racket[summon].
}
