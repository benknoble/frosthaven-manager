#lang scribble/manual

@(require (for-label (except-in racket null)
                     frosthaven-manager/defns
                     frosthaven-manager/parsers/formula))

@title{@tt{defns}}
@defmodule[frosthaven-manager/defns]

In this section, sections represented sections of the module, indicated by
comments.

@deftogether[(
              @defproc[(no-duplicates? [xs (listof any/c)]) boolean?]
              @defproc[(unique/c [c flat-contract?]) contract?]
              @defproc[(unique-with/c [key (-> any/c any/c)] [c flat-contract?]) contract?]
)]{
Utility procedures and contracts for verifying uniqueness. The predicate
@racket[no-duplicates?] is true if-and-only-if the input @racket[xs] has no
duplicates. The contract @racket[unique/c] requires that a value be a
@racket[(listof c)] and have no duplicates. The contract @racket[unique-with/c]
requires of a value @racket[v] that the result of @racket[(map key v)] is a
@racket[(unique/c c)].
}

@defproc[(vector-update! [v (and/c vector? (not/c immutable?))]
                         [pos natural-number/c]
                         [f (-> any/c any/c)])
         void?]{
Like @racket[list-update] for mutable @racket[vector]s. Equivalent to
setting @racket[v]@subscript{@racket[pos]} to @racket[f] of
@racket[v]@subscript{@racket[pos]}.
}

@section{Level Info}

@defstruct*[level-info
             ([monster-level natural-number/c]
              [gold natural-number/c]
              [trap-damage natural-number/c]
              [hazardous-terrain natural-number/c]
              [exp natural-number/c])
             #:transparent]{
An instance of @racket[level-info] exposes characteristics of the level, such as
the monster level, value of gold, damage caused by traps and hazardous terrain,
and end-of-scenario experience.
}

@defthing[number-of-levels natural-number/c]{
A constant representing the number of possible levels, as opposed to what the
levels are.
}

@defthing[max-level natural-number/c]{
A constant representing the maximum level. The minimum level is 0.
}

@defthing[level/c contract?]{
A contract recognizing valid level numbers, used for both the scenario level and
monster levels.
}

@defthing[max-players natural-number/c]{
A constant representing the maximum number of players.
}

@defthing[num-players/c contract?]{
A contract recognizing a valid number of players.
}

@defproc[(get-level-info [level level/c]) level-info?]{
Returns the @racket[level-info] for the given @racket[level] number.
}

@defproc[(inspiration-reward [num-players num-players/c]) natural-number/c]{
Returns the amount of inspiration rewarded for completing a scenario based on
how many players participated in the scenario.
}

@section{Players}

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

@section{Loot Deck}

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

@section{Scenario}

@deftogether[(
              @defthing[element? predicate/c]

              @defthing[fire element?]
              @defthing[ice element?]
              @defthing[air element?]
              @defthing[earth element?]
              @defthing[light element?]
              @defthing[dark element?]
)]{
The elements.

Serializable.
}

@deftogether[(
              @defthing[monster-modifier? predicate/c]

              @defthing[zero monster-modifier?]
              @defthing[minus1 monster-modifier?]
              @defthing[plus1 monster-modifier?]
              @defthing[minus2 monster-modifier?]
              @defthing[plus2 monster-modifier?]
              @defthing[null monster-modifier?]
              @defthing[crit monster-modifier?]
              @defthing[curse monster-modifier?]
              @defthing[bless monster-modifier?]
)]{
Monster modifier cards.

Serializable.
}

@deftogether[(
              @defthing[condition? predicate/c]

              @defthing[regenerate condition?]
              @defthing[ward condition?]
              @defthing[invisible condition?]
              @defthing[strengthen condition?]
              @defthing[wound condition?]
              @defthing[brittle condition?]
              @defthing[bane condition?]
              @defthing[poison condition?]
              @defthing[immobilize condition?]
              @defthing[disarm condition?]
              @defthing[impair condition?]
              @defthing[stun condition?]
              @defthing[muddle condition?]
)]{
The @racket[condition?] predicate recognizes all valid conditions, which are
listed here.

Serializable.
}

@deftogether[(
              @defproc[(discriminator:condition [c condition?]) integer?]
              @defproc[(selector:condition [i integer?]) condition?]
)]{
@tech[#:doc '(lib "rebellion/main.scrbl")]{Enum discriminator} and
@tech[#:doc '(lib "rebellion/main.scrbl")]{enum selector} for
@racket[condition?] values. Both contract error when the argument is outside the
appropriate domain.
}

@defproc[(initiative? [v any/c]) boolean?]{
A predicate recognizing valid initiative values.
}

@defthing[conditions (listof condition?)]{
All the conditions together.
}

@defthing[monster-modifier-deck (listof monster-modifier?)]{
A full deck of 20 monster modifier cards.
}

@deftogether[(
              @defthing[monster-curse-deck (listof monster-modifier?)]
              @defthing[bless-deck (listof monster-modifier?)]
)]{
Full decks of 10 monster curse and bless cards.
}

@defproc[(shuffle-modifier-deck? [deck (listof monster-modifier?)]) boolean?]{
True if-and-only-if @racket[deck] contains a @racket[null] or @racket[crit].
}

@deftogether[(
              @defproc[(better-modifier [a monster-modifier?]
                                        [b monster-modifier?])
                       monster-modifier?]
              @defproc[(worse-modifier [a monster-modifier?]
                                       [b monster-modifier?])
                       monster-modifier?]
)]{
Returns the better or worse of the two modifier cards.
}

@section{Monster Cards}

@defstruct*[monster-stats
             ([max-hp (or/c positive-integer? string?)]
              [move natural-number/c]
              [attack (or/c natural-number/c string?)]
              [bonuses (listof string?)]
              [effects (listof string?)]
              [immunities (listof string?)])
             #:prefab]{
The monster statistic representation, usually used with pre-fabs.
}

@defstruct*[monster-info
             ([set-name string?]
              [name string?]
              [normal-stats (apply list/c (build-list number-of-levels (const monster-stats?)))]
              [elite-stats (apply list/c (build-list number-of-levels (const monster-stats?)))])
             #:prefab]{
The monster information representation, often for reading pre-fab structs.
}

@defstruct*[monster-ability
             ([set-name string?]
              [name string?]
              [initiative initiative?]
              [abilities (listof string?)]
              [shuffle? boolean?]
              [location (or/c #f path?)])
             #:prefab]{
The monster ability representation, often for reading pre-fab structs.

Note that pre-fab syntax does not permit @racket[path?] objects.
}

@defthing[monster-number/c contract?]{
A contract that recognizes valid monster numbers.
}

@defstruct*[monster
             ([number monster-number/c]
              [elite? boolean?]
              [current-hp natural-number/c]
              [conditions (listof condition?)])
             #:transparent]{
A @racket[monster] captures the individual status of a monster, but not its
game statistics. Those are listed in its parent @racket[monster-group].

Prefer the smart constructor @racket[make-monster].

Serializable.
}

@defstruct*[monster-group
             ([set-name string?]
              [name string?]
              [level level/c]
              [normal-stats monster-stats?]
              [elite-stats monster-stats?]
              [monsters (listof monster?)])
             #:transparent]{
A @racket[monster-group] describes a group of @racket[monster]s and their stats.

Prefer the smart constructor @racket[make-monster-group] and the update
functions, which maintain an invariant of monsters sorted by eliteness and
number.

Serializable.
}

@defproc[(monster-stats-max-hp* [stats monster-stats?] [env env/c])
         positive-integer?]{
Calculates the maximum HP value of @racket[stats], which may be a formula.
}

@defproc[(monster-stats-attack* [stats monster-stats?] [env env/c])
         positive-integer?]{
Calculates the attack value of @racket[stats], which may be a formula.
}

@defproc[(monster-ability-name->text [ability (or/c #f monster-ability?)])
         string?]{
Returns a string suitable for display to indicate the name of a possibly-absent
monster ability.
}

@defproc[(monster-ability-initiative->text [ability (or/c #f monster-ability?)])
         string?]{
Returns a string suitable for display to indicate the initiative of a
possibly-absent monster ability.
}

@defproc[((monster-ability-ability->text [ability string?])
          [mg monster-group?] [env env/c])
         string?]{
Formats a single ability on a monster ability card, as from
@racket[monster-ability-abilities], by replacing keywords like ``Attack +1''
with calculated text and values.
}

@defproc[(monster-ability-ability->extras [ability-card (or/c #f monster-ability?)]
                                          [ability-text string?])
         (listof (or/c (list/c 'aoe-pict pict?)))]{
Returns a list of ``extras'' for rendering a specific @racket[ability-text] from
a @racket[monster-ability]. The meaning of each extra spec is as follows:

@itemlist[
          @item{an extra @racket[`(aoe-pict ,aoe-pict)] means the ability had an area of effect specified by the pict.}
]
}

@defproc[(make-monster [info monster-info?]
                       [level level/c]
                       [number monster-number/c]
                       [elite? boolean?]
                       [env env/c])
         monster?]{
Populates the resulting @racket[monster] based on the statistics from
@racket[info] and @racket[elite?]. Formulas are calculated using @racket[env].
}

@defproc[(make-monster-group [info monster-info?]
                             [level level/c]
                             [num+elite?s (and/c (listof (cons/c monster-number/c boolean?))
                                                 (unique-with/c car any/c))]
                             [env env/c])
         monster-group?]{
Creates a @racket[monter-group] at level @racket[level] based on the statistics
from @racket[info].

The @racket[num+elite?s] parameter provides a mapping from (unique) monster
numbers to their elite status. Only monster numbers in the mapping are added to
the @racket[monster-group].

Formulas are calculated using @racket[env].
}

@defproc[(get-monster-stats [mg monster-group?] [m monster?]) monster-stats?]{
Retrieves the corresponding @racket[monster-group-normal-stats] or
@racket[monster-group-elite-stats] based on @racket[(monster-elite? m)]. The
monster @racket[m] is assumed to be part of the group @racket[mg].
}

@defproc[(monster-at-max-health? [m monster?] [s monster-stats?] [env env/c]) boolean?]{
True if-and-only-if @racket[(monster-current-hp m)] is
@racket[(monster-stats-max-hp* s env)]. The stats @racket[s] are assumed to
correlate to the monster @racket[m].
}

@defproc[(monster-dead? [m monster?]) boolean?]{
True if-and-only-if @racket[(monster-current-hp m)] is @racket[0].
}

@defproc[((monster-update-condition [c condition?] [on? boolean?])
          [m monster?])
         monster?]{
Transforms @racket[(monster-conditions m)] by adding the condition
@racket[c] if @racket[on?] or removing it otherwise.
}

@defproc[((monster-update-hp [f (-> number? number?)])
          [m monster?])
         monster?]{
Transforms @racket[(monster-current-hp m)] by @racket[f]. If the result is not
@racket[positive?] the update is ignored.
}

@defproc[((monster-group-update-num
            [n monster-number/c]
            [f (-> monster? monster?)])
          [mg monster-group?])
         monster-group?]{
Transforms @racket[(monster-group-monsters mg)] by transforming the monster
numbered @racket[n] by @racket[f].
}

@defproc[((monster-group-remove [n monster-number/c])
          [mg monster-group?])
         monster-group?]{
Removes the monster numbered @racket[n] from @racket[(monster-group-monsters mg)].
}

@defproc[((monster-group-add [n monster-number/c] [elite? boolean?] [env env/c])
          [mg monster-group?])
          monster-group?]{
Adds the monster numbered @racket[n] to @racket[(monster-group-monsters mg)]. It
is elite if-and-only-if @racket[elite?] is @racket[#true].
}

@defproc[(monster-group-first-monster [mg monster-group?])
         (or/c #f monster-number/c)]{
The number of the first monster in the monster group @racket[mg], or
@racket[#false] if there are no such monsters.
}

@defproc[(monster->hp-text [m monster?] [ms monster-stats] [env env/c])
         string?]{
Formats the string @racket["HP: current/max"] for the monster @racket[m].
}

@defproc[(swap-monster-group-elites [mg monster-group?])
         monster-group?]{
The same monster group, but with all elite monsters normal and vice-versa.
}

@defproc[(swap-monster-elite [m monster?])
         monster?]{
The same monster, but normal instead of elite and vice-versa.
}
