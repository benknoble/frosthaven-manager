#lang scribble/manual

@(require (for-label (except-in racket null)
                     racket/gui/easy
                     racket/gui/easy/contract
                     qi
                     frosthaven-manager/defns
                     frosthaven-manager/monster-db
                     frosthaven-manager/parsers/formula
                     frosthaven-manager/manager))

@title{@tt{manager/state}}
@defmodule[frosthaven-manager/manager/state]

This module provides facilities for manipulating manager-level state.

@defstruct*[creature
             ([id any/c]
              [v (or/c player? monster-group*?)])
             #:transparent]{
A @racket[creature] is displayed in the central area of the Frosthaven Manager
GUI, as described in @secref{Creature_List}. Therefore a @racket[creature-v] can
be either a @racket[player] or a @racket[monster-group*].

A @racket[creature] is identified by its unique @racket[creature-id].

Serializable.
}

@defstruct*[monster-group*
             ([active (or/c #f monster-number/c)]
              [mg monster-group?])
             #:transparent]{
A @racket[monster-group*] wraps a @racket[monster-group] with a possibly active
@racket[monster-number/c], which identifies the monster currently displayed in
@secref{Monster_Group_Controls}.

Serializable.
}

@defstruct*[state
             ([|@|mode symbol?]
              [|@|level (obs/c level/c)]
              [|@|num-players (obs/c num-players/c)]
              [|@|creatures (obs/c (listof creature?))]
              [|@|cards-per-deck (obs/c (hash/c (listof loot-card?) natural-number/c))]
              [|@|loot-deck (obs/c (listof loot-card?))]
              [|@|num-loot-cards (obs/c natural-number/c)]
              [|@|elements (listof (obs/c element-state/c))]
              [|@|in-draw? (obs/c boolean?)]
              [|@|round (obs/c natural-number/c)]
              [|@|monster-modifier-deck (obs/c (listof monster-modifier?))]
              [|@|monster-discard (obs/c (listof monster-modifier?))]
              [|@|player-blesses (obs/c (listof monster-modifier?))]
              [|@|curses (obs/c (listof monster-modifier?))]
              [|@|blesses (obs/c (listof monster-modifier?))]
              [|@|modifier (obs/c (or/c #f monster-modifier?))]
              [|@|monster-prev-discard (obs/c (or/c #f monster-modifier?))]
              [|@|info-db (obs/c info-db/c)]
              [|@|ability-db (obs/c ability-db/c)]
              [|@|ability-decks (obs/c (hash/c string? ability-decks?))]
              [|@|stickers-per-loot-deck (obs/c (hash/c (listof loot-card?) natural-number/c))]
              [|@|prompts (obs/c (listof prompt/c))])]{
All of the "global" manager state.
}

@defproc[(make-state
           [|@|mode (maybe-obs/c symbol?) (|@| 'start)]
           [|@|level (maybe-obs/c level/c) (|@| 0)]
           [|@|num-players (maybe-obs/c num-players/c) (|@| 2)]
           [|@|creatures (maybe-obs/c (listof creature?)) (|@| empty)]
           [|@|cards-per-deck (maybe-obs/c (hash/c (listof loot-card?) natural-number/c)) (|@| (hash))]
           [|@|loot-deck (maybe-obs/c (listof loot-card?)) (|@| empty)]
           [|@|num-loot-cards (maybe-obs/c natural-number/c) (|@| 0)]
           [|@|elements (listof (maybe-obs/c element-state/c)) (make-states '(fire ice air earth light dark))]
           [|@|in-draw? (maybe-obs/c boolean?) (|@| #f)]
           [|@|round (maybe-obs/c natural-number/c) (|@| 1)]
           [|@|monster-modifier-deck (maybe-obs/c (listof monster-modifier?)) (|@| (shuffle monster-modifier-deck))]
           [|@|monster-discard (maybe-obs/c (listof monster-modifier?)) (|@| empty)]
           [|@|player-blesses (maybe-obs/c (listof monster-modifier?)) (|@| empty)]
           [|@|curses (maybe-obs/c (listof monster-modifier?)) (|@| monster-curse-deck)]
           [|@|blesses (maybe-obs/c (listof monster-modifier?)) (|@| bless-deck)]
           [|@|modifier (maybe-obs/c (or/c #f monster-modifier?)) (|@| #f)]
           [|@|monster-prev-discard (maybe-obs/c (or/c #f monster-modifier?)) (|@| #f)]
           [|@|info-db (maybe-obs/c info-db/c) (|@| (hash))]
           [|@|ability-db (maybe-obs/c ability-db/c) (|@| (hash))]
           [|@|ability-decks (maybe-obs/c (hash/c string? ability-decks?)) (|@| (hash))]
           [|@|stickers-per-loot-deck (maybe-obs/c (hash/c (listof loot-card?) natural-number/c)) (|@| (hash))]
           [|@|prompts (maybe-obs/c (listof prompt/c)) (|@| empty)])
         state?]{
Create an initial state.
}

@defproc[(state-|@|env [s state?]) (obs/c env/c)]{
Derives a formula environment observable from pieces of @racket[s].
}

@deftogether[(@defproc[(serialize-state [s state?] [out output-port?]) any]
              @defproc[(deserialize-state [in input-port?]) state?])]{
Procedures to serialize and deserialize a @racket[state?] value.
}

@defproc[(copy-state [from state?] [to state?]) any]{
Copies the state from @racket[from] to @racket[to] by updating the internal
observables. This makes it possible to update an existing @racket[state?] with
the values from a deserialized @racket[state?].
}

@deftogether[(
    @defproc[(make-undo [s state?]) obs?]
    @defproc[(undo! [s state?] [|@|undo obs?]) any]
    @defproc[(undoable? [undo list?]) any/c])]{
Undo procedures. To create an observable undo state that tracks @racket[s], use
@racket[(make-undo s)]. Then, when @racket[undoable?] is true of the observable
undo state, use @racket[(undo! s |@|undo)] to actually trigger change.

Warning: sometimes multiple undos are necessary to be coherent. Not all state
changes are recorded.
}

@defproc[(make-player-creature [i any/c]) creature?]{
Make a creature with @racket[creature-id] @racket[i] and @racket[creature-v] a
@racket[player?].
}

@deftogether[(
    @defproc[(update-players [creatures (listof creature?)]
                             [k any/c]
                             [f (-> player? player?)])
             (listof creature?)]
    @defproc[(update-monster-groups [creatures (listof creature?)]
                                    [k any/c]
                                    [f (-> monster-group? monster-group?)]
                                    [fn (-> (or/c #f monster-number/c) monster-group? (or/c #f monster-number/c))
                                        (flow 1>)])
             (listof creature?)]
)]{
Updates player or monster-group @racket[k] in @racket[creatures] via @racket[f].
When updating a monster-group, @racket[fn] can update the
@racket[monster-group*-active] number.
}

@deftogether[(
    @defproc[(update-all-players [creatures (listof creature?)]
                                 [f (-> player? player?)])
             (listof creature?)]
    @defproc[(update-all-monster-groups [creatures (listof creature?)]
                                        [f (-> monster-group? monster-group?)])
             (listof creature?)]
)]{
Updates all players or monster-groups in @racket[creatures] via @racket[f].
}

@deftogether[(
    @defproc[((update-player-name [s state?]) [k any/c] [name string?]) any]
    @defproc[((update-player-max-hp [s state?])
              [k any/c]
              [f (-> natural-number/c natural-number/c)])
             any]
)]{
Updates player @racket[k]s name to @racket[name] or max health via @racket[f].
}

@defproc[(creature-initiative [ads (hash/c string? ability-decks?)])
         (-> creature? (or/c +inf.0 initiative?))]{
Calculates a creature's initiative.
}

@deftogether[(
              @defthing[single-monster-event/c
                         contract?
                         #:value
                         (or/c
                           (list/c 'set 'from string? 'to string?)
                           (list/c 'monster 'from monster-info? 'to monster-info?)
                           (list/c 'include? monster-number/c 'to boolean?)
                           (list/c 'elite? monster-number/c 'to boolean?)
                           (list/c 'level level/c))]
              @defthing[add-monster-event/c
                         contract?
                         #:value (list/c 'add monster-group?)]
              @defthing[remove-monster-event/c
                         contract?
                         #:value (list/c 'remove monster-group?)]
)]{
Contracts for events used in callbacks to monster GUI views. Best used with
@racket[match].
}

@defproc[(add-or-remove-monster-group [s state?])
         (-> (or/c add-monster-event/c
                   remove-monster-event/c)
             any)]{
Adds or removes a monster group based on the received event.
}

@defproc[(draw-new-card-mid-round-if-needed [s state?] [set string?])
         any]{
If it is mid-round per @racket[state-@in-draw?] and the ability deck for
@racket[set] does not have a card per @racket[ability-decks-current], draw a
card for @racket[set].

It is the caller's responsibility to verify that a monster has been added and
needs to potentially trigger a new card.
}

@defproc[(initiative-public? [in-draw? boolean?]) boolean?]{
True if, according to @racket[in-draw?], initiative values should be publicly
revealed.
}

@defthing[element-state/c
           contract?
           #:value (or/c 'unfused 'infused 'waning)]{
A contract recognizing valid element states.
}

@defproc[(make-states [es (listof any/c)])
         (listof (obs/c element-state/c))]{
Builds an equally-sized list of element states to control @racket[es] in
@racket[elements-cycler].
}
