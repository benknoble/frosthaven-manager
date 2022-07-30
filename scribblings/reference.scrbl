#lang scribble/manual

@(require "common.rkt"
          (for-label
            (except-in racket
                       null)
            racket/gui/base
            (only-in xml xexpr?)
            rebellion/type/enum
            racket/gui/easy
            (except-in racket/gui/easy/operator
                       ~>
                       <~)
            racket/gui/easy/contract
            qi
            frosthaven-manager/defns
            (only-in frosthaven-manager/elements
                     size
                     element-pics
                     elements)
            frosthaven-manager/enum-helpers
            frosthaven-manager/gui/common-menu
            frosthaven-manager/gui/counter
            frosthaven-manager/gui/elements
            frosthaven-manager/gui/hierlist
            frosthaven-manager/gui/level-info
            frosthaven-manager/gui/loot-picker
            frosthaven-manager/gui/markdown
            frosthaven-manager/gui/mixins
            frosthaven-manager/gui/monsters
            frosthaven-manager/gui/player-info
            frosthaven-manager/gui/start
            frosthaven-manager/gui/static-table
            frosthaven-manager/manager
            frosthaven-manager/monster-db
            frosthaven-manager/observable-operator
            frosthaven-manager/qi
            frosthaven-manager/qi/list2hash
            ))

@title{Developer Reference}

None of these APIs should be considered stable enough for use in projects other
than Frosthaven Manager. They should be considered stable enough for use in
Frosthaven Manager. Changes to an internally-used API should be made with care
and compelling reason.

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

@subsection{GUI}

@defstruct*[creature
             ([id any/c]
              [v (or/c player? monster-group*?)])
             #:transparent]{
A @racket[creature] is displayed in the central area of the Frosthaven Manager
GUI, as described in @secref{Creature_List}. Therefore a @racket[creature-v] can
be either a @racket[player] or a @racket[monster-group*].

A @racket[creature] is identified by its unique @racket[creature-id].
}

@defstruct*[monster-group*
             ([active (or/c #f monster-number/c)]
              [mg monster-group?])
             #:transparent]{
A @racket[monster-group*] wraps a @racket[monster-group] with a possibly active
@racket[monster-number/c], which identifies the monster currently displayed in
@secref{Monster_Group_Controls}.
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
              [loot (listof loot-card?)])
             #:transparent]{
A @racket[player] captures everything about a player that Frosthaven Manager
needs.

You will not usually need the @racket[player] constructor: use the smart
constructor @racket[make-player] instead.
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

@subsection{Loot Deck}

@deftogether[(
              @defthing[material-kind? predicate/c]

              @defthing[lumber material-kind?]
              @defthing[metal material-kind?]
              @defthing[hide material-kind?]

              @defthing[material-kinds (listof material-kind?)]
)]{
Represents materials for loot cards.
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
}

@deftogether[(
              @defthing[random-item? predicate/c]
              @defthing[random-item random-item?]
)]{
Represents the random-item loot card.
}

@defstruct*[money ([amount (integer-in 1 3)])
                  #:transparent]{
Represents a loot card worth 1 to 3 gold.
}

@defstruct*[material
             ([name material-kind?]
              [amount (apply list/c (build-list max-players (const natural-number/c)))])
             #:transparent]{
Represents a loot card for a material; the amount varies by number of players.
}

@defstruct*[herb ([name herb-kind?])
                 #:transparent]{
Represents a loot card for an herb.
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
    @defthing[money-deck (listof money?)]
    @defthing[material-decks (hash/c material-kind? (listof material?))]
    @defthing[herb-decks (hash/c herb-kind? (listof herb?))]
)]{
Decks of loot cards from which you draw to make the loot deck.

Current values of some cards are random and will be adjusted when they are
known. Modifications via stickers are not yet supported.
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
              @defthing[monster-bless-deck (listof monster-modifier?)]
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
             ([max-hp positive-integer?]
              [move natural-number/c]
              [attack natural-number/c]
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
              [shuffle? boolean?])
             #:prefab]{
The monster ability representation, often for reading pre-fab structs.
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
}

@defproc[(make-monster [info monster-info?]
                       [level level/c]
                       [number monster-number/c]
                       [elite? boolean?])
         monster?]{
Populates the resulting @racket[monster] based on the statistics from
@racket[info] and @racket[elite?].
}

@defproc[(make-monster-group [info monster-info?]
                             [level level/c]
                             [num+elite?s (and/c (listof (cons/c monster-number/c boolean?))
                                                 (unique-with/c car any/c))])
         monster-group?]{
Creates a @racket[monter-group] at level @racket[level] based on the statistics
from @racket[info].

The @racket[num+elite?s] parameter provides a mapping from (unique) monster
numbers to their elite status. Only monster numbers in the mapping are added to
the @racket[monster-group].
}

@defproc[(get-monster-stats [mg monster-group?] [m monster?]) monster-stats?]{
Retrieves the corresponding @racket[monster-group-normal-stats] or
@racket[monster-group-elite-stats] based on @racket[(monster-elite? m)]. The
monster @racket[m] is assumed to be part of the group @racket[mg].
}

@defproc[(monster-at-max-health? [m monster?] [s monster-stats?]) boolean?]{
True if-and-only-if @racket[(monster-current-hp m)] is
@racket[(monster-stats-max-hp s)]. The stats @racket[s] are assumed to correlate
to the monster @racket[m].
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

@defproc[((monster-group-add [n monster-number/c] [elite? boolean?])
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

@defthing[elements
           (listof element-pics?)]{
All of the elements bundled together. This module also provides bindings from
the names of the elemnts to @racket[element-pics] values, but they are not
documented here. See @secref{Elements_Tracker} for the various element names and
pictures.
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
)]{
Menu items for Frosthaven Manager.
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
           [es (listof element-pics?)]
           [panel (unconstrained-domain-> (is-a?/c view<%>)) hpanel])
         (values (listof (obs/c element-state/c))
                 (is-a?/c view<%>))]{
Returns both a list of observables controlling element states and a GUI view
displaying the @racket[element-pics].
}

@defproc[(wane-element [state element-state/c])
         element-state/c]{
Returns the new element state after waning for one cycle.
}

@subsection{@tt{gui/hierlist}}
@defmodule[frosthaven-manager/gui/hierlist]

@defproc[(hierlist
           [|@item| (maybe-obs/c hierlist/c)]
           [#:min-size |@min-size| (maybe-obs/c size/c) '(#f #f)]
           [#:stretch |@stretch| (maybe-obs/c stretch/c) '(#t #t)]
           [#:margin |@margin| (maybe-obs/c margin/c) '(0 0)]
           [#:inset |@inset| (maybe-obs/c margin/c) '(5 5)]
           [#:style style
            (listof (one-of/c 'no-border 'control-border 'combo
                              'no-hscroll 'no-vscroll
                              'hide-hscroll 'hide-vscroll
                              'auto-vscroll 'auto-hscroll
                              'resize-corner 'deleted 'transparent))
            '(no-hscroll)])
         (is-a?/c view<%>)]{
Produces a GUI view of a hierarchical list. The format of @racket[|@item|]
described by @racket[hierlist/c] is not yet stable and therefore not documented
here.
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

@defproc[(loot-picker
           [#:on-card on-card
            (-> (or/c (list/c 'add (listof loot-card?))
                      (list/c 'remove predicate/c))
                any)
            void])
         (is-a?/c view<%>)]{
A GUI view to build a loot deck by including certain loot cards. The callback
@racket[on-card] is invoked with an "event" that specifies either a deck of
cards from which one should be added or a predicate to identify the card to
remove.
}

@defproc[((loot-picker-updater
            [|@loot-deck| (obs/c (listof loot-card?))])
          [evt (or/c (list/c 'add (listof loot-card?))
                     (list/c 'remove predicate/c))])
         any]{
Updates the observable @racket[|@loot-deck|] based on the event @racket[evt] as
described in @racket[loot-picker] by picking a random card from the deck or
removing a card matching the predicate, then shuffling.
}

@defproc[(loot-button
           [|@loot-deck| (obs/c (listof loot-card?))]
           [|@num-loot-cards| (obs/c natural-number/c)]
           [|@num-players| (obs/c natural-number/c)]
           [|@players| (obs/c (listof creature?))]
           [#:on-close on-close (-> any) void]
           [#:on-player on-player (-> any/c any) void])
         (is-a?/c view<%>)]{
A GUI view of a button that, when clicked, shows a view to assign the top loot
card from @racket[|@loot-deck|] to one of @racket[|@players|] via buttons. The
callback @racket[on-close] is invoked when the view is closed and can be used
to, @italic{e.g.}, remove the top loot card from the deck. The callback
@racket[on-player] is invoked with the @racket[creature-id] of the player from
@racket[|@players|] whose button is clicked to assign loot; it can be used to,
@italic{e.g.}, assign the loot card. After @racket[on-player] is invoked, the
view is closed, which invokes @racket[on-close].

See @secref{Scenario_Information_and_Loot} for how @racket[loot-button]
functions in Frosthaven Manager.
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
           [#:on-select on-select (-> (or/c #f monster-number/c) any) void]
           [#:on-condition on-condition (-> monster-number/c condition? boolean? any) void]
           [#:on-hp on-hp (-> monster-number/c (-> number? number?) any) void]
           [#:on-kill on-kill (-> monster-number/c any) void]
           [#:on-new on-new (-> monster-number/c boolean? any)])
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
]
}

@defproc[(db-view [|@info-db| (obs/c info-db/c)]
                  [|@ability-db| (obs/c ability-db/c)])
         (is-a?/c view<%>)]{
A GUI view to display the hierarchical monster database, separated by
@racket[monster-info] and @racket[monster-ability].
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
           [|@num-players| (obs/c num-players/c)]
           [#:on-condition on-condition (-> (list/c condition? boolean?) any) void]
           [#:on-hp on-hp (-> (-> number? number?) any) void]
           [#:on-xp on-xp (-> (-> number? number?) any) void]
           [#:on-initiative on-initiative (-> number? any) void])
         (is-a?/c view<%>)]{
A GUI view of a single player. See @secref{Player_Controls}. The callback
@racket[on-condition] is given an condition and value that determines whether
the condition should be applied (@racket[#true]) or removed (@racket[#false]).
The callbacks @racket[on-hp] and @racket[on-xp] are given procedures to modify
@racket[player-current-hp] and @racket[player-xp], respectively. The callback
@racket[on-initiative] is given a new initiative for @racket[player-initiative].
The number of players is used to format the player's loot appropriately.
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

@section{@tt{manager}}
@defmodule[frosthaven-manager/manager]

This module's main function is to run the Frosthaven Manager. It provides only
a single binding:

@defproc[(render-manager) renderer?]{
Renders the Frosthaven Manager and returns the @racket[renderer?].
}

@section{@tt{monster-db}}
@defmodule[frosthaven-manager/monster-db]

See @secref{Editing_Monster_Information} for more information on custom monster
databases.

@deftogether[(
              @defthing[info-db/c contract?]
              @defthing[ability-db/c contract?]
)]{
Contracts recognizing monster databases of @racket[monster-info] and
@racket[monster-ability] values.
}

@defproc[(get-dbs [db-file path-string?])
         (values info-db/c ability-db/c)]{
Reads @racket[db-file] and produces the monster databases.
}

@defthing[default-monster-db path-string?]{
The demo, default monster database included with Frosthaven Manager.
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
