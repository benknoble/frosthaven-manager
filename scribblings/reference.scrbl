#lang scribble/manual

@(require "common.rkt"
          (for-label
            (except-in racket
                       null)
            "../defns.rkt"
            (only-in "../elements.rkt"
                     size
                     element-pics
                     elements)
            "../enum-helpers.rkt"
            "../gui/common-menu.rkt"
            "../gui/counter.rkt"
            "../gui/elements.rkt"
            "../gui/hierlist.rkt"
            "../gui/level-info.rkt"
            "../gui/loot-picker.rkt"
            "../gui/markdown.rkt"
            "../gui/mixins.rkt"
            "../gui/monsters.rkt"
            "../gui/player-info.rkt"
            "../gui/start.rkt"
            "../gui/static-table.rkt"
            "../manager.rkt"
            "../monster-db.rkt"
            "../observable-operator.rkt"
            "../qi.rkt"
            ))

@title{Developer Reference}

None of these APIs should be considered stable enough for use in projects other
than Frosthaven Manager. They should be considered stable enough for use in
Frosthaven Manager. Changes to an internally-used API should be made with care
and compelling reason.

@section{@tt{defns}}
@defmodule[frosthaven-manager/defns]

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

@subsection{GUI}

@defstruct*[creature
             ([id any/c]
              [v (or/c player? monster-group*?)])]{
A @racket[creature] is displayed in the central area of the Frosthaven Manager
GUI, as described in @secref{Creature_List}. Therefore a @racket[creature-v] can
be either a @racket[player] or a @racket[monster-group*].

A @racket[creature] is identified by its unique @racket[creature-id].
}

@defstruct*[monster-group*
             ([active (or/c #f monster-number/c)]
              [mg monster-group?])]{
A @racket[monster-group*] wraps a @racket[monster-group] with a possibly active
@racket[monster-number/c], which identifies the monster currently displayed in
@secref{Monster_Group_Controls}.
}

@subsection{Level Info}

@defthing[number-of-levels natural-number/c]{
A constant representing the number of possible levels, as opposed to what the
levels are.
}

@defthing[level/c contract?]{
A contract recognizing valid level values, used for both the scenario level and
monster levels.
}

@subsection{Players}

@defstruct*[player
             ([name string?]
              [max-hp positive-integer?]
              [current-hp natural-number/c]
              [xp natural-number/c]
              [conditions (listof condition?)]
              [initiative initiative?]
              [loot (listof loot-card?)])]{
A @racket[player] captures everything about a player that Frosthaven Manager
needs.

You will not usually need the @racket[player] constructor: use the smart
constructor @racket[make-player] instead.
}

@defproc[(make-player [name string?] [max-hp positive-integer?]) player?]{
Creates a @racket[player] with @racket[name] and @racket[max-hp].
}

@subsection{Loot Deck}

@defthing[loot-card? predicate/c]{
This predicate recognizes valid loot cards. It is also a valid
@racket[contract?].
}

@subsection{Scenario}

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

@subsection{Monster Cards}

@defstruct*[monster-stats
             ([max-hp positive-integer?]
              [move natural-number/c]
              [attack natural-number/c]
              [bonuses (listof string?)]
              [effects (listof string?)]
              [immunities (listof string?)])]{
The monster statistic representation, usually used with pre-fabs.
}

@defstruct*[monster-info
             ([set-name string?]
              [name string?]
              [normal-stats (apply list/c (build-list number-of-levels (const monster-stats?)))]
              [elite-stats (apply list/c (build-list number-of-levels (const monster-stats?)))])]{
The monster information representation, often for reading pre-fab structs.
}

@defstruct*[monster-ability
             ([set-name string?]
              [name string?]
              [initiative initiative?]
              [abilities (listof string?)]
              [shuffle? boolean?])]{
The monster ability representation, often for reading pre-fab structs.
}

@defthing[monster-number/c contract?]{
A contract that recognizes valid monster numbers.
}

@defstruct*[monster
             ([number monster-number/c]
              [elite? boolean?]
              [current-hp natural-number/c]
              [conditions (listof condition?)])]{
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
              [monsters (listof monster?)])]{
A @racket[monster-group] describes a group of @racket[monster]s and their stats.

Prefer the smart constructor @racket[make-monster-group].
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

@section{@tt{elements}}
@defmodule[frosthaven-manager/elements]

@section{@tt{enum-helpers}}
@defmodule[frosthaven-manager/enum-helpers]

@section[#:tag "frosthaven-manager/gui"]{@tt{gui}}

The following sections describe modules under @tt{frosthaven-manager/gui}.

@subsection{@tt{gui/common-menu}}
@defmodule[frosthaven-manager/gui/common-menu]

@subsection{@tt{gui/counter}}
@defmodule[frosthaven-manager/gui/counter]

@subsection{@tt{gui/elements}}
@defmodule[frosthaven-manager/gui/elements]

@subsection{@tt{gui/hierlist}}
@defmodule[frosthaven-manager/gui/hierlist]

@subsection{@tt{gui/level-info}}
@defmodule[frosthaven-manager/gui/level-info]

@subsection{@tt{gui/loot-picker}}
@defmodule[frosthaven-manager/gui/loot-picker]

@subsection{@tt{gui/markdown}}
@defmodule[frosthaven-manager/gui/markdown]

@subsection{@tt{gui/mixins}}
@defmodule[frosthaven-manager/gui/mixins]

@subsection{@tt{gui/monsters}}
@defmodule[frosthaven-manager/gui/monsters]

@subsection{@tt{gui/player-info}}
@defmodule[frosthaven-manager/gui/player-info]

@subsection{@tt{gui/start}}
@defmodule[frosthaven-manager/gui/start]

@subsection{@tt{gui/static-table}}
@defmodule[frosthaven-manager/gui/static-table]

@section{@tt{manager}}
@defmodule[frosthaven-manager/manager]

@section{@tt{monster-db}}
@defmodule[frosthaven-manager/monster-db]

@section{@tt{observable-operator}}
@defmodule[frosthaven-manager/observable-operator]

@section{@tt{qi}}
@defmodule[frosthaven-manager/qi]

