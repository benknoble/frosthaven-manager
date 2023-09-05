#lang scribble/manual

@(require "common.rkt"
          (for-label
            (except-in racket
                       null)
            racket/serialize
            racket/gui/base
            (only-in pict
                     pict?
                     cc-superimpose
                     ghost)
            (only-in xml xexpr?)
            rebellion/type/enum
            racket/gui/easy
            (except-in racket/gui/easy/operator
                       ~>
                       <~)
            racket/gui/easy/contract
            qi
            megaparsack
            (prefix-in pretty: pretty-expressive)
            frosthaven-manager/aoe-images
            frosthaven-manager/defns
            (only-in frosthaven-manager/elements
                     size
                     element-pics element-pics?
                     elements)
            frosthaven-manager/enum-helpers
            frosthaven-manager/manager
            frosthaven-manager/gui/common-menu
            frosthaven-manager/gui/counter
            frosthaven-manager/gui/elements
            frosthaven-manager/gui/font
            frosthaven-manager/gui/formula-editor
            frosthaven-manager/gui/helpers
            frosthaven-manager/gui/level-info
            frosthaven-manager/gui/loot-picker
            frosthaven-manager/gui/manager
            frosthaven-manager/gui/markdown
            frosthaven-manager/gui/mixins
            frosthaven-manager/gui/monsters
            frosthaven-manager/gui/player-info
            frosthaven-manager/gui/render
            frosthaven-manager/gui/server
            frosthaven-manager/gui/stacked-tables
            frosthaven-manager/gui/start
            frosthaven-manager/gui/static-table
            frosthaven-manager/monster-db
            frosthaven-manager/parsers/foes
            frosthaven-manager/parsers/formula
            frosthaven-manager/parsers/monster
            frosthaven-manager/observable-operator
            frosthaven-manager/pp/bestiary
            frosthaven-manager/qi
            frosthaven-manager/qi/list2hash
            (prefix-in server: frosthaven-manager/server)
            (only-in frosthaven-manager/syntax/module-reader)
            frosthaven-manager/syntax/monsters
            ))

@title{Developer Reference}

None of these APIs should be considered stable enough for use in projects other
than Frosthaven Manager. They should be considered stable enough for use in
Frosthaven Manager. Changes to an internally-used API should be made with care
and compelling reason.

@section{@tt{aoe}}

This module implements the Area-of-Effect (AoE) language. See
@secref{Programming_a_Scenario} and @racketmodname[frosthaven-manager/aoe]
for more information.

@section{@tt{aoe-images}}
@defmodule[frosthaven-manager/aoe-images]

This module provides procedures for constructing area-of-effect diagrams.

@defparam[hex-size size natural-number/c #:value 30]{
The size of the hexes built from this module.
}

@defproc[(r) (and/c positive? number?)]{
Returns the amount by which odd rows need shifted to align with even rows in a
hex-grid (based on @racket[hex-size]).
}

@deftogether[(
              @defproc[(S) pict?]
              @defproc[(X) pict?]
              @defproc[(O) pict?]
              @defproc[(M) pict?]
)]{
Hexes for an area-of-effect diagram: respectively, spacers, attacks, allies, and
the initiating figure.
}

@defproc[(border-size [max-row natural-number/c] [max-col natural-number/c])
         (and/c positive? number?)]{
Returns the side-length of a square rectangle which would encompass an
area-of-effect diagram of @racket[max-row] rows and @racket[max-col] columns in
a hex-grid, if the diagram were centered and superimposed on the rectangle à la
@racket[cc-superimpose].
}

@deftogether[(
              @defthing[spec-sym? flat-contract? #:value (or/c 's 'x 'o 'm 'g)]
              @defthing[spec?
                         flat-contract?
                         #:value
                         (listof (list/c exact-positive-integer?
                                         boolean?
                                         (listof (list/c spec-sym? natural-number/c))))]
              @defproc[(spec->shape [s spec?]) pict?]
)]{
Convert an AoE spec to a shape. The spec contains a list of rows; each row
contains a line number, a flag indicating this line should be offset relative
to the lines above and below it (which are not necessarily in the spec), and a
list of column specifiers, pairing symbols with columns in sorted order.

The symbols represent the corresponding shapes, with @racket['g] a
@racket[ghost] hex.
}

@deftogether[(
              @defthing[syntaxes-can-be-spec? predicate/c]
              @defproc[(syntaxes->spec [stxs (and/c (listof syntax?) syntaxes-can-be-spec?)])
                       spec?]
)]{
Convert a list of syntax objects to a @racket[spec?].
}

@defproc[(string->spec [s string?]) spec?]{
Uses @racket[syntaxes->spec] on syntax read from @racket[s] by
@racket[read-syntax] to produce an AoE spec. Fails if the resulting syntaxes
cannot be a spec, as defined by @racket[syntaxes-can-be-spec?].
}

@section{@tt{defns}}
@defmodule[frosthaven-manager/defns]

In this section, subsections represented subsections of the module, indicated by
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

@subsection{Level Info}

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

@subsection{Players}

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

@subsection{Loot Deck}

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

@subsection{Scenario}

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

@subsection{Monster Cards}

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

@section{@tt{elements}}
@defmodule[frosthaven-manager/elements]

@defthing[size natural-number/c]{
The size of the element pictures.
}

@defstruct*[element-pics
             ([name string?]
              [infused pict?]
              [waning pict?]
              [unfused pict?])
             #:transparent]{
A container for a named set of element pictures.
}

@defproc[(elements) (listof element-pics?)]{
Returns all of the elements bundled together. This module also provides bindings
from the names of the elemnts to procedures returning @racket[element-pics]
values, but they are not documented here. See @secref{Elements_Tracker} for the
various element names and pictures.
}

@section{@tt{enum-helpers}}
@defmodule[frosthaven-manager/enum-helpers]

@defproc[(make-property-maker-that-displays-as-constant-names
           [desc uninitialized-enum-descriptor?])
         (listof (cons/c struct-type-property? any/c))]{
This helper adjusts @tech[#:doc '(lib "rebellion/main.scrbl")]{enum types} so
that the @racket[display] string is the same as the constant name as in
@racket[define-enum-type].
}

@defproc[(compose-property-makers [p (-> uninitialized-enum-descriptor? (listof (cons/c struct-type-property? any/c)))] ...)
         (-> uninitialized-enum-descriptor? (listof (cons/c struct-type-property? any/c)))]{
Creates a property maker suitable for @racket[define-enum-type] that combines
each @racket[p].
}

@defform[(define-serializable-enum-type id (constant-id ...) enum-option ...)
         #:grammar ([enum-option #:omit-root-binding
                                 (code:line #:descriptor-name descriptor-id)
                                 (code:line #:predicate-name predicate-id)
                                 (code:line #:discriminator-name discriminator-id)
                                 (code:line #:selector-name selector-id)
                                 (code:line #:property-maker prop-maker-expr)
                                 (code:line #:inspector inspector-expr)])
         #:contracts ([prop-maker-expr (-> uninitialized-enum-descriptor?
                                           (listof (cons/c struct-type-property? any/c)))]
                      [inspector-expr inspector?])]{
Exactly like @racket[define-enum-type], but with the addition of
@racket[prop:serializable] via a deserialize-info named
@racketidfont{deserialize-info:}@racket[id].
}

@section{@tt{manager}}
@defmodule[frosthaven-manager/manager]

This module reprovides all the bindings from
@racketmodname[frosthaven-manager/manager/state],
@racketmodname[frosthaven-manager/manager/ability-decks],
@racketmodname[frosthaven-manager/manager/modifier-decks],
@racketmodname[frosthaven-manager/manager/db], and
@racketmodname[frosthaven-manager/manager/loot].

@subsection{@tt{manager/state}}
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
              [|@|stickers-per-loot-deck (obs/c (hash/c (listof loot-card?) natural-number/c))])]{
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
           [|@|stickers-per-loot-deck (obs/c (hash/c (listof loot-card?) natural-number/c)) (|@| (hash))])
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

@subsection{@tt{manager/ability-decks}}
@defmodule[frosthaven-manager/manager/ability-decks]

@defstruct*[ability-decks ([current (or/c #f monster-ability?)]
                           [draw (listof monster-ability?)]
                           [discard (listof monster-ability?)])
                          #:transparent]{
Monster ability deck, with currently active card, draw pile, and discard pile.

Serializable.
}

@defproc[(ability-decks-draw-next [ad ability-decks?]) ability-decks?]{
Draws a card from the ability deck.
}

@defproc[(ability-decks-discard-and-maybe-shuffle [ad ability-decks?])
         ability-decks?]{
Discards the active card and shuffles the ability deck if necessary.
}

@defproc[(update-ability-decks [f (-> ability-decks? ability-decks?)])
         (-> (hash/c string? ability-decks?) (hash/c string? ability-decks?))]{
Updates each deck via @racket[f].
}

@defproc[(move-top-draw-to-bottom [ad ability-decks?]) ability-decks?]{
Moves the top card of the draw pile to its bottom.
}

@subsection{@tt{manager/modifier-decks}}
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

@subsection{@tt{manager/db}}
@defmodule[frosthaven-manager/manager/db]

This module provides facilities for manipulating the active monster databases.

@defproc[(init-dbs [db path-string?] [s state?]) any]{
Initialize the active monster databases.
}

@defproc[(init-dbs-and-foes [db path-string?] [s state?]) any]{
Initialize the active monster databases, exactly as @racket[init-dbs].
Additionally, initialize the foes from @racket[db] if it provides a foes
specification. This manipulates @racket[(state-@creatures s)]; see also
@racket[add-or-remove-monster-group] and
@racketmodname[frosthaven-manager/foes].
}

@subsection{@tt{manager/loot}}
@defmodule[frosthaven-manager/manager/loot]

This module provides facilities for manipulating the loot deck.

@defproc[(update-loot-deck-and-num-loot-cards [s state?])
         (-> (list/c (or/c 'add 'remove) (listof loot-card?)) any)]{
Update the loot deck based on the loot-picker event.
}

@defproc[((give-player-loot [s state?]) [k any/c]) any]{
Give player @racket[k] the top loot card.
}

@defproc[(place-loot-on-bottom [s state?]) any]{
Rotate the top loot card to the bottom of the deck.
}

@section[#:tag "frosthaven-manager/gui"]{@tt{gui}}

The following sections describe modules under @tt{frosthaven-manager/gui}.

@subsection{@tt{gui/common-menu}}
@defmodule[frosthaven-manager/gui/common-menu]

@deftogether[(
    @defthing[about-menu-item (-> (is-a?/c view<%>))]
    @defthing[issue-menu-item (-> (is-a?/c view<%>))]
    @defthing[feature-menu-item (-> (is-a?/c view<%>))]
    @defthing[contribute-menu-item (-> (is-a?/c view<%>))]
    @defthing[send-feedback-menu-item (-> (is-a?/c view<%>))]
    @defthing[how-to-play-menu-item (-> (is-a?/c view<%>))]
    @defthing[launch-server-menu-item (-> (is-a?/c view<%>))]
)]{
Menu items for Frosthaven Manager.
}

@defproc[(do-about) renderer?]{
Renders an About window, as in @racket[about-menu-item]. Useful with
@racket[application-about-handler].
}

@subsection{@tt{gui/counter}}
@defmodule[frosthaven-manager/gui/counter]

@defproc[(counter [|@label| (maybe-obs/c string?)]
                  [up (-> any)]
                  [down (-> any)])
         (is-a?/c view<%>)]{
A GUI component for a counter with a label and up and down callbacks.
}

@subsection{@tt{gui/elements}}
@defmodule[frosthaven-manager/gui/elements]

@defthing[element-state/c
           contract?
           #:value (or/c 'unfused 'infused 'waning)]{
A contract recognizing valid element states.
}

@defproc[(elements-cycler
           [|@|states (listof (obs/c element-state/c))]
           [es (listof element-pics?)]
           [panel (unconstrained-domain-> (is-a?/c view<%>)) hpanel])
         (is-a?/c view<%>)]{
Returns a GUI view displaying the @racket[element-pics]. Each element of
@racket[es] is controlled by the corresponding element of @racket[|@|states].
}

@defproc[(make-states [es (listof any/c)])
         (listof (obs/c element-state/c))]{
Builds an equally-sized list of element states to control @racket[es] in
@racket[elements-cycler].
}

@deftogether[(@defproc[(infuse-all [es (listof (obs/c element-state/c))]) any]
              @defproc[(consume-all [es (listof (obs/c element-state/c))]) any])]{
Set all element states @racket[es] to @racket['infused] or @racket['unfused],
respectively.
}

@defproc[(wane-element [state element-state/c])
         element-state/c]{
Returns the new element state after waning for one cycle.
}

@defproc[(transition-element-state [state element-state/c]) element-state/c]{
Returns the new element state after cycling once, with unfused wrapping around
to infused.
}

@subsection{@tt{gui/font}}
@defmodule[frosthaven-manager/gui/font]

This module provides helpers for manipulating font objects, as in
@racket[font%].

@defproc[(copy-font
           [f (is-a?/c font%)]
           [#:size size (real-in 0.0 1024.0) (send f get-size size-in-pixels?)]
           [#:face face (or/c string? #f) (send f get-face)]
           [#:family family (or/c 'default 'decorative 'roman 'script 'swiss 'modern 'symbol 'system)
            (send f get-family)]
           [#:style style (or/c 'normal 'italic 'slant) (send f get-style)]
           [#:weight weight font-weight/c (send f get-weight)]
           [#:underlined? underlined? any/c (send f get-underlined)]
           [#:smoothing smoothing (or/c 'default 'partly-smoothed 'smoothed 'unsmoothed)
            (send f get-smoothing)]
           [#:size-in-pixels? size-in-pixels? any/c (send f get-size-in-pixels)]
           [#:hinting hinting (or/c 'aligned 'unaligned) (send f get-hinting)]
           [#:feature-settings feature-settings font-feature-settings/c
            (send f get-feature-settings)]
           [#:font-list font-list (or/c (is-a?/c font-list%) #f) (current-font-list)])
         (is-a?/c font%)]{
Copy all the features of font @racket[f] to a brand new font object. Supply
modified values via the keyword arguments.
}

@defthing[big-control-font (is-a?/c font%)]{
A font bigger than @racket[normal-control-font] and italic, but otherwise the
same.
}

@subsection{@tt{gui/formula-editor}}
@defmodule[frosthaven-manager/gui/formula-editor]

This module provides GUI objects for interactive formula editing.

@defproc[(formula-editor [|@|env env/c]) (is-a?/c view<%>)]{
A window containing an interactive formula editor.
}

@defproc[(formula-menu-item [|@|env env/c]) (is-a?/c view<%>)]{
A menu item that displays an interactive formula editor.
}

@subsection{@tt{gui/helpers}}
@defmodule[frosthaven-manager/gui/helpers]

@defproc[(translate-to-top-coords
           [this (is-a?/c area<%>)]
           [top (is-a?/c area<%>)]
           [x position-integer?]
           [y position-integer?])
         (values position-integer? position-integer?)]{
Returns translated @racket[x] and @racket[y] coordinates relative to
@racket[top], assuming they were originally relative to @racket[this].
}

@subsection{@tt{gui/level-info}}
@defmodule[frosthaven-manager/gui/level-info]

@defproc[(level-stats
           [|@level| (obs/c level/c)]
           [|@num-players| (obs/c num-players/c)])
         (is-a?/c view<%>)]{
A GUI view that displays the @racket[level-info] corresponding to
@racket[|@level|] and @racket[|@num-players|].
}

@defproc[(level-table [|@level| (obs/c level/c)])
         (is-a?/c view<%>)]{
A GUI view of a button that shows a table of @racket[level-info] values for each
level. The current @racket[|@level|] starts selected.
}

@defproc[(inspiration-table [|@num-players| (obs/c num-players/c)])
         (is-a?/c view<%>)]{
A GUI view of a button that shows a table of inspiration rewards for each
possible number of players. The current @racket[|@num-players|] starts selected.
}

@subsection{@tt{gui/loot-picker}}
@defmodule[frosthaven-manager/gui/loot-picker]

@defproc[(loot-picker [#:on-card on-card (-> (list/c (or/c 'add 'remove) (listof loot-card?)) any) void]
                      [#:on-sticker on-sticker (-> (list/c (or/c 'add 'remove) (listof loot-card?)) any) void])
         (is-a?/c view<%>)]{
A GUI view to build a loot deck by including certain loot cards. The callback
@racket[on-card] is invoked with an "event" that specifies a deck of cards from
which one card should be added or removed. Similarly for @racket[on-sticker] to
add stickers to decks.
}

@defproc[((loot-picker-updater [|@cards-per-loot-deck| (obs/c (hash/c (listof loot-card?) natural-number/c))])
          [evt (list/c (or/c 'add 'remove) (listof loot-card?))])
         any]{
Updates the observable @racket[|@cards-per-loot-deck|] based on the event
@racket[evt] as described in @racket[loot-picker] by updating the count of cards
per deck.
}

@defproc[((update-stickers-per-deck [|@stickers-per-deck| (obs/c (hash/c (listof loot-card?) natural-number/c))])
          [evt (list/c (or/c 'add 'remove) (listof loot-card?))])
         any]{
Updates the observable @racket[|@stickers-per-deck|] based on the event
@racket[evt] as described in @racket[loot-picker] by updating the count of
stickers per deck.
}

@defproc[(build-loot-deck [cards-per-loot-deck (hash/c (listof loot-card?) natural-number/c)]
                          [stickers-per-loot-deck (hash/c (listof loot-card?) natural-number/c)])
         (listof loot-card?)]{
Converts a count of cards per deck into an shuffled deck of loot cards. This can
be considered the interpreter for a language whose values are like those
produced by combined @racket[loot-picker], @racket[loot-picker-updater], and
@racket[update-stickers-per-deck]; namely, mappings from decks to number of
cards.
}

@defproc[(loot-button
           [|@loot-deck| (obs/c (listof loot-card?))]
           [|@num-loot-cards| (obs/c natural-number/c)]
           [|@num-players| (obs/c num-players/c)]
           [|@players| (obs/c (listof (cons/c player? any/c)))]
           [#:on-player on-player (-> any/c any) void]
           [#:on-top on-top (-> any) void]
           [#:on-bottom on-bottom (-> any) void])
         (is-a?/c view<%>)]{
A GUI view of a button that, when clicked, shows a view to assign the top loot
card from @racket[|@loot-deck|] to one of @racket[|@players|] via buttons. The
callback @racket[on-player] is invoked with the ID (@racket[cdr]) of the player
from @racket[|@players|] whose button is clicked to assign loot; it can be used
to, @italic{e.g.}, assign the loot card. After @racket[on-player] is invoked,
the view is closed.

Additionally, buttons for the top and bottom of the deck trigger the
@racket[on-top] and @racket[on-bottom] callbacks, which then also close the
view.

See @secref{Scenario_Information_and_Loot} for how @racket[loot-button]
functions in Frosthaven Manager.
}

@defproc[(loot-preview [|@loot-deck| (obs/c (listof loot-card?))]
                       [|@num-players| (obs/c num-players/c)])
         (is-a?/c view<%>)]{
A button that, when clicked, shows a loot deck previewer.
}

@subsection{@tt{gui/manager}}
@defmodule[frosthaven-manager/gui/manager]

This module's main function is to run the Frosthaven Manager. It provides only
a single binding:

@defproc[(manager [s state?]) (is-a?/c window-view<%>)]{
A view for the Frosthaven Manager. Render with @racket[render/eventspace].
}

@subsection{@tt{gui/markdown}}
@defmodule[frosthaven-manager/gui/markdown]

@defproc[(markdown-text
           [|@content| (maybe-obs/c (or/c string? path?))]
           [#:min-size @min-size (maybe-obs/c size/c) '(#f #f)]
           [#:stretch @stretch (maybe-obs/c stretch/c) '(#t #t)]
           [#:margin @margin (maybe-obs/c margin/c) '(0 0)]
           [#:inset @inset (maybe-obs/c margin/c) '(5 5)]
           [#:style style
            (listof (one-of/c 'no-border 'control-border 'combo
                              'no-hscroll 'no-vscroll
                              'hide-hscroll 'hide-vscroll
                              'auto-vscroll 'auto-hscroll
                              'resize-corner 'deleted 'transparent))
            '(no-hscroll)])
         (is-a?/c view<%>)]{
A GUI view rendering the markdown in @racket[|@content|], which is either a
@tech[#:doc ref-doc]{string} of Markdown or a path to a file containing Markdown.
The view updates when @racket[|@content|] does---note that in the string case
this means the Markdown content has changed, but in the path case this means the
path has changed, not the contents of the file at the path!

The following Markdown features are supported:
@itemlist[
          @item{Paragraphs;}
          @item{HTML comments;}
          @item{Hyperlinks;}
          @item{Blockquotes;}
          @item{Unordered and ordered lists;}
          @item{Horizontal rules;}
          @item{@bold{Bold}, @italic{italic}, and @tt{code} styles;}
          @item{and six levels of headings.}
]

The following @racket[xexpr?]s are supported recursively in the parsed Markdown;
these map to the Markdown features above:
@itemlist[
          @item{Any @tech[#:doc ref-doc]{string}}
          @item{Any expression tagged @tt{!HTML-COMMENT}, the tag for HTML comments}
          @item{Any expression tagged @tt{a}}
          @item{Any expression tagged @tt{blockquote}}
          @item{Any expression tagged @tt{ul}}
          @item{Any expression tagged @tt{ol}}
          @item{Any expression tagged @tt{li}}
          @item{Any expression tagged @tt{hr}}
          @item{Any expression tagged @tt{p}}
          @item{Any expression tagged @tt{strong}}
          @item{Any expression tagged @tt{em}}
          @item{Any expression tagged @tt{code}}
          @item{Any expression tagged @tt{h1}}
          @item{Any expression tagged @tt{h2}}
          @item{Any expression tagged @tt{h3}}
          @item{Any expression tagged @tt{h4}}
          @item{Any expression tagged @tt{h5}}
          @item{Any expression tagged @tt{h6}}
]
Any other tag found in the parsed Markdown is a runtime error.

Note that Markdown technically requires 4 spaces or a single tab as leading
indent for nesting lists and other blocks; while many Markdown implementations
(such as those used on GitHub) are more lenient, the implementation backing
@racket[markdown-text] is stricter on this point.
}

@subsection{@tt{gui/mixins}}
@defmodule[frosthaven-manager/gui/mixins]

@defproc[(make-closing-proc-mixin [out (-> (-> any) any)])
         (make-mixin-contract top-level-window<%>)]{
Produces a @tech[#:doc ref-doc]{mixin} that calls @racket[out] on instantiation
with a procedure that closes the window. Many uses of @racket[out] are to store
a local binding to this "close" procedure.
}

@defproc[(make-on-close-mixin [proc (-> any)])
         (make-mixin-contract top-level-window<%>)]{
Produces a @tech[#:doc ref-doc]{mixin} that @racket[augment]s @racket[on-close]
to call @racket[proc].
}

@defform[(define-close! close!-id set-close-mixin-id)]{
If the mixin @racket[set-close-mixin-id] is applied to a
@racket[top-level-window<%>] then @racket[close!-id] is a nullary procedure that
closes it.
}

@subsection{@tt{gui/monsters}}
@defmodule[frosthaven-manager/gui/monsters]

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

@defproc[(single-monster-picker
           [info-db info-db/c]
           [|@initial-level| (obs/c level/c)]
           [#:on-change on-change (-> single-monster-event/c any) void]
           [#:unavailable unavailable (set/c string?) empty])
         (is-a?/c view<%>)]{
A GUI view used to build a monster group by choosing the set, name, and included
monsters (along with their elite status and level). The available choices come
from @racket[info-db] less @racket[unavailable] (a set of monster names). The
callback @racket[on-change] is invoked each time changes are made. The default
monster level is specified by @racket[|@initial-level|].
}

@defproc[(simple-monster-group-view [|@mg| (obs/c monster-group?)])
         (is-a?/c view<%>)]{
A GUI view of a monster group showing a table of monsters and some other
information about the group.
}

@defproc[(multi-monster-picker
           [info-db info-db/c]
           [|@initial-level| (obs/c level/c)]
           [|@env| (obs/c env/c)]
           [#:on-change on-change (-> (or/c add-monster-event/c
                                            remove-monster-event/c)
                                      any) void])
         (is-a?/c view<%>)]{
A GUI view used to choose the monsters in a scenario: it composes
@racket[single-monster-picker] in order to allow selection and removal of entire
groups. The callback @racket[on-change] is invoked to notify of the addition or
removal of a group. Other parameters are used as in
@racket[single-monster-picker].
}

@defproc[(monster-group-view
           [|@mg| (obs/c monster-group?)]
           [|@ability| (obs/c (or/c #f monster-ability?))]
           [|@monster-num| (obs/c (or/c #f monster-number/c))]
           [|@env| (obs/c env/c)]
           [#:on-select on-select (-> (or/c #f monster-number/c) any) void]
           [#:on-condition on-condition (-> monster-number/c condition? boolean? any) void]
           [#:on-hp on-hp (-> monster-number/c (-> number? number?) any) void]
           [#:on-kill on-kill (-> monster-number/c any) void]
           [#:on-new on-new (-> monster-number/c boolean? any)]
           [#:on-swap on-swap (-> (or/c 'all monster-number?) any)])
         (is-a?/c view<%>)]{
A GUI view used to display an entire monster group. See
@secref{Monster_Group_Controls}. The @racket[|@ability|] is displayed if an
ability card is present. The @racket[|@monster-num|] determines the currently
selected monster in the detailed portion of the view.

The callbacks function as follows:
@itemlist[
          @item{@racket[on-select] is given a new monster number when one is selected in the detailed view, or @racket[#false] if there are none.}
          @item{@racket[on-condition] is given a monster number, condition, and either @racket[#true] or @racket[#false] to indiciate whether the condition should be applied or removed.}
          @item{@racket[on-hp] is given a monster number and a procedure to update the monsters @racket[monster-current-hp].}
          @item{@racket[on-kill] is invoked with a monster number when that monster is killed.}
          @item{@racket[on-new] is invoked with a monster number and @racket[#true] if the monster is elite or @racket[#false] otherwise for a newly added monster.}
          @item{@racket[on-swap] is invoked with @racket['all] if all monsters should be swapped by @racket[swap-monster-group-elites], or with a monster number if only that monster should be swapped by @racket[swap-monster-elite].}
]
}

@defproc[(db-view [|@info-db| (obs/c info-db/c)]
                  [|@ability-db| (obs/c ability-db/c)]
                  [|@monster-groups| (obs/c (listof monster-group?))])
         (is-a?/c view<%>)]{
A GUI view to display the hierarchical monster database, separated by
@racket[monster-info] and @racket[monster-ability].

Any pre-set monster groups will also be shown.
}

@defproc[(add-monster-group [|@info-db| (obs/c info-db/c)]
                            [|@initial-level| (obs/c level/c)]
                            [|@monster-names| (obs/c (set/c string? #:cmp 'dont-care #:kind 'dont-care))]
                            [|@env| (obs/c env/c)]
                            [#:on-group on-group (-> monster-group? any) void])
         any]{
Renders a dialog to add a monster group by invoking the callback
@racket[on-group] if one is selected. The value of @racket[|@initial-level|] is
used for the initial level of the group, which can be adjusted in the dialog.
Similarly, @racket[|@monster-names|] specifies which names are not available for
the new group.

Originally an internal part of the implementation of
@racket[multi-monster-picker] until it had uses in the main playing view.
}

@subsection{@tt{gui/player-info}}
@defmodule[frosthaven-manager/gui/player-info]

@defproc[(player-input-views
           [|@num-players| (obs/c natural-number/c)]
           [#:on-name on-name (-> natural-number/c string? any) void]
           [#:on-hp on-hp (-> natural-number/c (-> number? number?) any) void]
           [#:names names (or/c #f (listof string?)) #f]
           [#:hps hps (or/c #f (listof positive-integer?)) #f])
         (is-a?/c view<%>)]{
A GUI view to enter player names and max HP. The number of entry slots is
determined by @racket[|@num-players|]. The callbacks @racket[on-name] and
@racket[on-hp] are invoked with a player number and a name or a procedure to
modify @racket[player-max-hp]. Default names and max HP values can be specified
via @racket[names] and @racket[hps].
}

@defproc[(player-view
           [|@player| (obs/c player?)]
           [#:on-condition on-condition (-> (list/c condition? boolean?) any) void]
           [#:on-hp on-hp (-> (-> number? number?) any) void]
           [#:on-xp on-xp (-> (-> number? number?) any) void]
           [#:on-initiative on-initiative (-> number? any) void]
           [#:on-summon add-summon (-> string? positive-integer? any)]
           [#:on-summon-hp update-summon-hp (-> natural-number/c (-> number? number?) any)]
           [#:on-summon-condition update-summon-condition (-> natural-number/c (list/c condition? boolean?) any)]
           [#:kill-summon kill-summon (-> natural-number/c any)])
         (is-a?/c view<%>)]{
A GUI view of a single player. See @secref{Player_Controls}. The callback
@racket[on-condition] is given an condition and value that determines whether
the condition should be applied (@racket[#true]) or removed (@racket[#false]).
The callbacks @racket[on-hp] and @racket[on-xp] are given procedures to modify
@racket[player-current-hp] and @racket[player-xp], respectively. The callback
@racket[on-initiative] is given a new initiative for @racket[player-initiative].
The number of players is used to format the player's loot appropriately.

The summon callbacks are given the summon number, a list index, to indicate
which summon to update. Adding a summon is done by name and max HP.
}

@subsection{@tt{gui/render}}
@defmodule[frosthaven-manager/gui/render]

@defparam[current-renderer
           r (or/c #f renderer?)
           #:value #f]{
A parameter for the current renderer. This can be set so that sub-views can
access the top-level renderer. Note that it is not re-entrant, in the sense that
to make it effective one must render an application by
@codeblock{
(define root (render ...))
(current-renderer root)
}
Any other application running in the same thread cannot use
@racket[current-renderer] or it will interfere with the previous application.
This also holds more generally of sub-views @racket[render]ed on-the-fly. See
@racket[render/eventspace] to avoid this.

This will not affect multiple applications built and run separately that use
this library, since they're in separate processes completely.
}

@defproc[(render/eventspace [tree (is-a?/c view<%>)]
                            [#:parent parent (or/c #f render?) #f]
                            [#:eventspace es eventspace? (current-eventspace)])
         renderer?]{
Renders (as in @racket[render]) @racket[tree] with parent @racket[parent] in the
eventspace @racket[es], then queues a high-priority callback in the
handler-thread for @racket[es] to set @racket[current-renderer] to the resulting
renderer, which is returned.

Pass a new @tech[#:doc gui-doc]{eventspace} created with
@racket[make-eventspace] to separate the rendered @racket[tree] and
corresponding @racket[current-renderer] from other applications.

This can be used to group windows in an application together, but note that
subsequent calls with the same @racket[es] will override that eventspace's
@tech[#:doc gui-doc]{handler thread}'s @racket[current-renderer].

For a short-lived window that should tear down the eventspace on closure,
combine with @racket[with-closing-custodian/eventspace].
}

@deftogether[(
             @defform[(with-closing-custodian/eventspace e ...+)]
             @defform[#:id closing-custodian closing-custodian]
             @defform[#:id closing-eventspace closing-eventspace]
             @defform[#:id close-custodian-mixin close-custodian-mixin]
             )]{
Evaluates the body expressions @racket[e ...] with the following special
variables available:
@itemize[
         @item{@racket[closing-custodian] is a new @tech[#:doc ref-doc]{custodian} that manages @racket[closing-eventspace].}
         @item{@racket[closing-eventspace] is a new @tech[#:doc gui-doc]{eventspace} managed by @racket[closing-custodian].}
         @item{@racket[close-custodian-mixin] is a new @tech[#:doc ref-doc]{mixin} for @racket[top-level-window<%>]s that causes @racket[closing-custodian] to shutdown after the corresponding window is closed.}
         ]
For example, the following produces either @racket[#t] or @racket[#f] depending
on whether window A or window B was closed first. Note also the use of
@racket[render/eventspace] to set @racket[current-renderer] correctly.
@codeblock{
(require racket/gui/easy)
(define main-es (make-eventspace))
(render/eventspace #:eventspace main-es (window #:title "A" (text "A")))
(define aux-es
  (with-closing-custodian/eventspace
    (render/eventspace
      #:eventspace closing-eventspace
      (window #:mixin close-custodian-mixin
              #:title "B"
              (text "B")))
    closing-eventspace))

(sync main-es) ;; wait until window A is closed
(eventspace-shutdown? aux-es) ;; true if window B was closed first
}
}

@subsection{@tt{gui/server}}
@defmodule[frosthaven-manager/gui/server]

@defproc[(launch-server [s state?]) renderer?]{
Renders a window in a new closing eventspace with server information, and
launches a server. See @racket[with-closing-custodian/eventspace] and
@racket[server:launch-server].
}

@subsection{@tt{gui/stacked-tables}}
@defmodule[frosthaven-manager/gui/stacked-tables]

@defproc[(stacked-tables
           [#:topleft? topleft? boolean? #t]
           [#:panel panel (-> (is-a?/c view<%>) ... (is-a?/c view<%>)) hpanel]
           [|@data| (obs/c (vectorof any/c))]
           [final-view (-> (obs/c (or/c #f any/c)) (is-a?/c view<%>))]
           [column1 column?]
           [column-spec column?] ...)
         (is-a?/c view<%>)]{
A view of @racket[|@data|] using stacked tables. The tables are horizontal,
left-to-right by default. Supplying @racket[vpanel] for @racket[panel] makes the
stack vertical. When @racket[topleft?] is true, the first table is on the left
or top of the stack. Otherwise it is on the right or bottom, reversing the order
of subsequent tables.

The stack of tables is determined by @racket[column1] and each
@racket[column-spec]. The first is always @racket[column1].

Starting with @racket[|@data|] and @racket[column1], a table is added to the
stack. The table's title is given by @racket[column-title]. The labels for the
items in the table come from applying @racket[column-entry->label] to the values
in the data. When a value is selected, the data for the next table and
@racket[column-spec] is produced by @racket[column-entry->next] on the
selection. This value is automatically wrapped in @racket[vector] as needed.

This process continues, adding tables to the stack whose data depends on
previous data and selections, until the final table and @racket[column-spec] are
added. The final selection, which is @emph{not} automatically vectorized, is
given to @racket[final-view]. The resulting view is also added to the stack.

The intermediate data produced by @racket[column-entry->next] is automatically
emptied when no value is selected previously. In contrast, @racket[final-view]
needs to handle the case that no data has yet been selected. A common pattern
is to compute a default value:
@codeblock|{
(stacked-tables
  @data
  (λ (@x?) ... (@~> @x? (or _ default)) ...)
  ...)
}|
}

@defstruct*[column ([title string?]
                    [entry->label (-> any/c string?)]
                    [entry->next (-> any/c (or/c any/c (vectorof any/c)))])]{
A column specification for @racket[stacked-tables], which explains how the
specification is used.

A note about @racket[column-entry->next]: you almost certainly want to return a
@racket[vector] for all but (possibly) the last @racket[column.] Intermediate
@racket[column]s likely have multiple choices. As a convenience, when there is
only one, you may omit the vector. For the final @racket[column], you likely
want to omit the vector unless the selected data is one: the data here is the
final selection, of which there should probably be one.
}

@subsection{@tt{gui/start}}
@defmodule[frosthaven-manager/gui/start]

@defproc[(start-view
           [#:on-level on-level (-> level/c any) void]
           [#:on-player on-player (-> num-players/c any) void])
         (is-a?/c view<%>)]{
A GUI view for the start screen of Frosthaven Manager. The callbacks are invoked
with the level and number of players for each update to those values.
}

@subsection{@tt{gui/static-table}}
@defmodule[frosthaven-manager/gui/static-table]

@defproc[(static-table
           [columns (listof label-string?)]
           [num-rows natural-number/c]
           [entry->columns (listof (-> any/c any/c))]
           [#:index->entry index->entry (-> natural-number/c natural-number/c) values]
           [#:entry->value entry->value (-> natural-number/c any/c) values]
           [#:selection |@selection| (maybe-obs/c
                                       (or/c #f
                                             exact-nonnegative-integer?
                                             (listof exact-nonnegative-integer?)))
            #f]
           [#:widths widths
            (maybe-obs/c
              (or/c #f
                    (listof
                      (or/c (list/c exact-nonnegative-integer?
                                    dimension-integer?)
                            (list/c exact-nonnegative-integer?
                                    dimension-integer?
                                    dimension-integer?
                                    dimension-integer?)))))])
         (is-a?/c view<%>)]{
A GUI view for static tables. The columns are labelled by @racket[columns], and
there are exactly @racket[num-rows] rows. Each row is indexed by a natural
number @racket[i] from @racket[0] to @racket[(sub1 num-rows)];
@racket[(entry->value (index->entry i))] computes a value @racket[v] on which
the functions in @racket[entry->columns] are called to compute the values of the
columns for that row. Each row is labelled with the entry @racket[(index->entry i)].

Summarizing: each row is indexed by a natural number in the range
[0,@racket[num-rows]). An entry is computed by @racket[index->entry]. A value is
computed from the entry by @racket[entry->value]. From this value, functions in
@racket[entry->columns] compute the elements of the row.

The selection is determined by @racket[|@selection|] as with @racket[table].

The column widths are calculated automatically based on @racket[columns], or are
provided as @racket[widths].
}

@section{@tt{bestiary}}

This module implements the bestiary language. See
@secref{Programming_a_Scenario} and
@racketmodname[frosthaven-manager/bestiary] for more information.

@section{@tt{monster-db}}
@defmodule[frosthaven-manager/monster-db]

See @secref{Programming_a_Scenario} for more information on custom monster
databases.

@deftogether[(
              @defthing[info-db/c contract?]
              @defthing[ability-db/c contract?]
)]{
Contracts recognizing monster databases of @racket[monster-info] and
@racket[monster-ability] values.
}

@defproc[(datums->dbs [xs (listof any/c)])
         (values info-db/c ability-db/c)]{
Filters the @racket[monster-info] and @racket[monster-ability] values out of
@racket[xs] and produces monster databases.
}

@defproc[(get-dbs [db-file path-string?])
         (values info-db/c ability-db/c)]{
Reads @racket[db-file] and produces the monster databases.
}

@defthing[default-monster-db path-string?]{
The demo, default monster database included with Frosthaven Manager.
}

@section{@tt{parsers}}

@subsection{@tt{parsers/foes}}
@defmodule[frosthaven-manager/parsers/foes]

This module contains parsers for @(hash-lang)
@racketmodname[frosthaven-manager/foes]. See
@secref{Programming_a_Scenario} for more details.

@defproc[(parse-foes [src any/c] [in input-port?] [#:syntax? syn? any/c])
         (or/c syntax? foes/pc)]{
The result is @racket[syntax?] with source @racket[src] if @racket[syn?] is
true, and the datum it contains matches @racket[foes/pc].
}

@deftogether[(@defthing[foes/pc flat-contract? #:value (listof (or/c (list/c 'import string?) monster-info? (listof monster-ability?) foe/pc))]
              @defthing[foe/pc flat-contract? #:value (list/c string? string? numbering/pc (listof spec/pc))]
              @defthing[spec/pc flat-contract? #:value (hash/c num-players/c monster-type/pc #:immutable #t)]
              @defthing[numbering/pc flat-contract? #:value (or/c "ordered" "random" #f)]
              @defthing[monster-type/pc flat-contract? #:value (or/c "absent" "normal" "elite")])]{
Contracts for foes values.
}

@deftogether[(@defthing[foes/p (parser/c char? foes/pc)]
              @defthing[foe/p (parser/c char? foe/pc)])]{
Textual parsers for parts of the foes language.
}

@subsection{@tt{parsers/formula}}
@defmodule[frosthaven-manager/parsers/formula]

This module contains parsers for arithmetic formulas over addition, subtraction,
multiplication, division, rounding, and a limited set of variables. The parse
result is a function from an environment of variables to a number.

@deftogether[(
              @defthing[env/c flat-contract? #:value (hash/c (or/c "L" "C") number? #:flat? #t)]
              @defthing[expr/pc contract? #:value (-> env/c number?)]
)]{
Contracts for the parse results of formulas.
}

@defthing[expr/p (parser/c char? expr/pc)]{
Textual parser for formulas.
}

@defproc[(parse-expr [in string?]) expr/pc]{
Parses a string as a formula or fails.
}

@subsection{@tt{parsers/monster}}
@defmodule[frosthaven-manager/parsers/monster]

This module contains parsers for @(hash-lang)
@racketmodname[frosthaven-manager/bestiary]. See
@secref{Programming_a_Scenario} for more details.

@defproc[(parse-bestiary [src any/c] [in input-port?] [#:syntax? syn? any/c])
         (or/c syntax? bestiary/c)]{
The result is @racket[syntax?] with source @racket[src] if @racket[syn?] is
true, and the datum it contains matches @racket[bestiary/c].
}

@defthing[bestiary/c flat-contract?
                     #:value
                     (listof (or/c (list/c 'import string?)
                                   monster-info?
                                   (listof monster-ability?)))]{
A contract for bestiary values.
}

@deftogether[(@defthing[monster/p (parser/c char? monster-info?)]
              @defthing[ability-deck/p (parser/c char? (listof monster-ability?))]
              @defthing[import-monsters/p (parser/c char? (list/c 'import string?))]
              @defthing[bestiary/p (parser/c char? bestiary/c)])]{
Textual parsers for parts of the bestiary language.
}

@defproc[(bestiary-dupes [xs (listof any/c)])
         (values (or/c #f (listof string?))
                 (or/c #f (listof string?)))]{
Returns duplicate monster names from bestiaries and ability decks in
@racket[xs]. The first value is based on any @racket[monster-info]s and the
second on @racket[monster-ability] decks.
}

@section{@tt{observable-operator}}
@defmodule[frosthaven-manager/observable-operator]

In addition to the shorthands below, this module exports @racket[define/obs],
@racket[|@|], @racket[:=], and @racket[λ:=] from
@racketmodname[racket/gui/easy/operator].

@defproc[(|<@| [|@o| obs?] [f (-> any/c any/c)]) any/c]{
An alias for @racket[obs-update!].
}

@defproc[(|@>| [|@o| obs?] [f (-> any/c any/c)]) obs?]{
An alias for @racket[obs-map].
}

@defproc[(|λ<@| [|@o| obs?] [f (-> any/c any/c)]) (-> any/c)]{
An alias for @racket[λ<~].
}

@defproc[(|@!| [|@o| obs?]) any/c]{
An alias for @racket[obs-peek].
}

@defform[(|@~>| |@o| flo)
         #:contracts ([|@o| obs?])]{
An alias for @racket[obs-map] that wraps @racket[flo] in @racket[flow].
}

@defform[(|<~@| |@o| flo)
         #:contracts ([|@o| obs?])]{
An alias for @racket[obs-update!] that wraps @racket[flo] in @racket[flow].
}

@section{@tt{pp}}

This collection holds pretty printers based on
@racketmodname[pretty-expressive].

@subsection{@tt{pp/bestiary}}
@defmodule[frosthaven-manager/pp/bestiary]

This module pretty-prints bestiary files. It can be run as a program with

@terminal|{
racket -l- frosthaven-manager/pp/bestiary
}|

to format standard in or a provided file to standard out. Use @DFlag{help} for
more options.

@defproc[(pretty-bestiary [bestiary bestiary/c] [#:lang-line? lang-line? any/c #t]) pretty:doc?]{
Creates a document for pretty printing from the results of a parsed bestiary.
The document starts with a @(hash-lang) line preceding the result if
@racket[lang-line?] is not @racket[#f].
}

@section{@tt{qi}}
@defmodule[frosthaven-manager/qi]

This modules provides everything from @racketmodname[qi] in addition to the
bindings from @racketmodname[frosthaven-manager/qi/list2hash].

@subsection{@tt{qi/list2hash}}
@defmodule[frosthaven-manager/qi/list2hash]

@defform[(list~>hash maybe->key maybe->value)
          #:grammar
          [(maybe->key (code:line)
                       (code:line #:->key ->key-flo))
           (maybe->value (code:line)
                         (code:line #:->value ->value-flo))]]{
This Qi form transforms the input value (a list) into a hash, where each element
of the list is mapped into a key via @racket[->key-flo] and a value via
@racket[->value-flo]. It uses @racket[list->hash] as implementation.
}

@defproc[(list->hash [xs list?]
                     [#:->key ->key (-> any/c any/c) identity]
                     [#:->value ->value (-> any/c any/c) identity])
         hash?]{
Transforms @racket[xs] to a hash by mapping each element into a key via
@racket[->key] and a value via @racket[->value].
}

@section{@tt{server}}
@defmodule[frosthaven-manager/server]

@defproc[(launch-server [s state?] [send-event procedure?])
         (values string? (-> any))]{
Launches the actual web server for @racket[s]. The callback protocol for
@racket[send-event] is not yet formalized and very unstable.

Returns the server address (on a best-guess basis) and a @code{stop} procedure
that stops the server when called.
}

@section{@tt{syntax}}

The modules in this collection provide helpers for macros, syntax, and
languages.

@subsection{@tt{syntax/module-reader}}
@defmodule[frosthaven-manager/syntax/module-reader]

This expander language wraps @racketmodname[syntax/module-reader] by assuming a
specific reading protocol.

This module does not have a reader of its own, so should be used with
@racket[module] or @(hash-lang) @racketmodname[s-exp].

@defform[#:literals (from)
         (#%module-begin expander-mod-path
          [parser-id from parser-mod-path])]{
The following example demonstrates the entire grammer of the expander language:
@codeblock|{
#lang s-exp frosthaven-manager/syntax/module-reader
frosthaven-manager/foes
[parse-foes from frosthaven-manager/parsers/foes]
}|

Or with @racket[module]:
@codeblock|{
#lang racket
(module reader frosthaven-manager/syntax/module-reader
  frosthaven-manager/foes
  [parse-foes from frosthaven-manager/parsers/foes])
}|

The semantics are as follows. The resulting module satisfies the language reader
extension protocol from @secref["parse-reader" #:doc '(lib "scribblings/reference/reference.scrbl")]
via @racketmodname[syntax/module-reader] with a few specifications. The
@racket[expander-mod-path] is used as in @racketmodname[syntax/module-reader] to
determine the module-path for the initial bindings of modules produced by the
reader. The @racket[parser-id], which must be provided by
@racket[parser-mod-path], is assumed to parse the whole body as with the
@racket[#:whole-body-readers?] keyword for @racketmodname[syntax/module-reader].
In addition, it should support the following protocol: the parser accepts 2
positional arguments. The first is the same name-value as @racket[read-syntax];
the second is the same input port as for @racket[read] and @racket[read-syntax]
with line-counting enabled. Then it must accept a keyword option
@racket[#:syntax?], whose value is a boolean indicating whether or not to
produce a syntax object.

Examples of valid parsers include @racket[parse-foes] and
@racket[parse-bestiary].
}

@subsection{@tt{syntax/monsters}}
@defmodule[frosthaven-manager/syntax/monsters]

@defform[#:literals (provide import info ability)
         (make-dbs (provide info-db-id ability-db-id)
                   (import import-mod-path ...)
                   (info monster-info ...)
                   (ability (monster-ability ...) ...))
         #:contracts ([monster-info monster-info?]
                      [monster-ability monster-ability?])]{
Binds and provides @racket[info-db-id] and @racket[ability-db-id] to
@racket[info-db/c] and @racket[ability-db/c] values, respectively, by importing
all the monster information from each @racket[import-mod-path] and merging it
with the provided @racket[monster-info] and @racket[monster-ability].

Each @racket[import-mod-path] is expected to provide the same
@racket[info-db-id] and @racket[ability-db-id].

The @racket[provide] keyword in the provide specification is recognized by
binding and must be the same as the one from @racketmodname[racket/base]. The
@racket[import], @racket[info], and @racket[ability] keywords are recognized by
datum identity.
}

@defproc[(syntaxes->bestiary-parts [syntaxes (listof syntax?)])
         (list/c (listof syntax?) (listof syntax?) (listof syntax?) (listof syntax?))]{
Separates the input @racket[syntaxes] into 4 categories: imports whose datum
matches @racket[(list/c 'import string?)], monster information matching
@racket[monster-info?], ability decks matching @racket[(listof monster-ability?)],
and foes matching @racket[foe/pc].
}

@defproc[(imports->dbs [import-paths (listof string?)])
         (values (listof info-db/c) (listof ability-db/c))]{
Produces all the monster information databases, one for each import in
@racket[import-paths], using @racket[get-dbs].
}

@defproc[(check-monsters-have-abilities
           [imported-info-dbs (listof info-db/c)]
           [imported-ability-dbs (listof ability-db/c)]
           [infos (listof monster-info?)]
           [actions (listof monster-ability?)])
         boolean?]{
True iff the set names among all the given @racket[imported-info-dbs] and
@racket[infos] is a subset of those among all the given
@racket[imported-ability-dbs] and @racket[actions].
}

@defproc[(check-monsters-have-abilities-message
           [imported-info-dbs (listof info-db/c)]
           [imported-ability-dbs (listof ability-db/c)]
           [infos (listof monster-info?)]
           [actions (listof monster-ability?)])
         string?]{
An error message for when @racket[check-monsters-have-abilities] fails.
}

@defproc[(check-foes-have-monsters
           [imported-info-dbs (listof info-db/c)]
           [infos (listof monster-info?)]
           [foes (listof foe/pc)])
         boolean?]{
True iff the foe names among all the given @racket[foes] is a subset of the
monster names among all the given @racket[imported-info-dbs] and @racket[infos].
}

@defproc[(check-foes-have-monsters-message
           [imported-info-dbs (listof info-db/c)]
           [infos (listof monster-info?)]
           [foes (listof foe/pc)])
         string?]{
An error message for when @racket[check-foes-have-monsters] fails.
}
