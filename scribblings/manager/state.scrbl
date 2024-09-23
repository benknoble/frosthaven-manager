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
GUI, as described in @secref{Creature_list}. Therefore a @racket[creature-v] can
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
@secref{Monster_group_controls}.

Serializable.
}

@defproc[(creature-is-mg*? [c creature?]) any/c]{
True iff @racket[c] holds a @racket[monster-group*].
}

@defstruct*[state
             ([|@|mode symbol?]
              [|@|level (obs/c level/c)]
              [|@|num-players (obs/c num-players/c)]
              [|@|creatures (obs/c (listof creature?))]
              [|@|type->number-of-cards (obs/c (hash/c loot-type/c natural-number/c))]
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
              [|@|bestiary-path (obs/c (or/c #f path-string?))]
              [|@|ability-decks (obs/c (hash/c string? ability-decks?))]
              [|@|prompts (obs/c (listof prompt/c))]
              [|@|type->deck (maybe-obs/c (hash/c loot-type/c (listof loot-card?)))])]{
All of the "global" manager state.
}

@defproc[(make-state
           [|@|mode (maybe-obs/c symbol?) (|@| 'play)]
           [|@|level (maybe-obs/c level/c) (|@| 0)]
           [|@|num-players (maybe-obs/c num-players/c) (|@| 2)]
           [|@|creatures (maybe-obs/c (listof creature?)) (|@| empty)]
           [|@|type->number-of-cards (maybe-obs/c (hash/c loot-type/c natural-number/c)) (|@| (hash))]
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
           [|@|bestiary-path (maybe-obs/c (or/c #f path-string?)) (|@| #f)]
           [|@|ability-decks (maybe-obs/c (hash/c string? ability-decks?)) (|@| (hash))]
           [|@|prompts (maybe-obs/c (listof prompt/c)) (|@| empty)]
           [|@|type->deck (maybe-obs/c (hash/c loot-type/c (listof loot-card?))) (|@| standard-loot-deck)])
         state?]{
Create an initial state.
}

@defproc[(state-|@|env [s state?]) (obs/c env/c)]{
Derives a formula environment observable from pieces of @racket[s].
}

@deftogether[(@defproc[(state-|@|info-db [s state?]) (obs/c info-db/c)]
              @defproc[(state-|@|ability-db [s state?]) (obs/c ability-db/c)])]{
Derive corresponding databases from @racket[state-|@|bestiary-path]. The
databases are empty if the path is not present.
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
    @defproc[(undo? [v any/c]) boolean?]
    @defproc[(make-undo [s state?]) undo?]
    @defproc[(undo! [s state?] [u undo?]) any]
    @defproc[(undoable? [u undo?]) (obs/c boolean?)])]{
Undo procedures. To create an observable undo state that tracks @racket[s], use
@racket[(make-undo s)]. Then, when @racket[undoable?] is true of the observable
undo state, use @racket[(undo! s _undo)] to actually trigger change.

Warning: sometimes multiple undos are necessary to be coherent. Not all state
changes are recorded.
}

@defproc[(make-player-creature [i any/c]) creature?]{
Make a creature with @racket[creature-id] @racket[i] and @racket[creature-v] a
@racket[player?].
}

@defproc[(setup-players [s state?]) any]{
Update @racket[(state-|@|creatures s)] based on the number of players
@racket[(state-|@|num-players s)]: new blank players are added if the number of
actual players is smaller than the requested number. Any extras are removed if
the actual number is larger than the requested number.
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

@defproc[(kill-monster [s state?] [monster-group-id any/c] [monster-number monster-number/c])
         any]{
Composes @racket[monster-group-remove] with @racket[update-monster-groups], and
intelligently sets the current active number or removes the monster group if the
last monster is killed. Prefer this procedure to the lower-level primitives when
manipulating state to remove or kill monsters in monster groups.
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

@defproc[(add-prompt [s state?]) (-> prompt/c any)]{
Add a prompt to @racket[s].
}

@defproc[((remove-prompt [s state?]) [i natural-number/c] [p prompt/c]) any]{
Remove a prompt @racket[p] from @racket[s] by index @racket[i]. If the prompt at
index @racket[i] is not @racket[p], no prompts are removed.
}
