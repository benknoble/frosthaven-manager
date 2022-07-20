#lang scribble/manual

@(require "common.rkt"
          (for-label
            (except-in racket
                       null)
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
            ))

@title{Developer Reference}

None of these APIs should be considered stable enough for use in projects other
than Frosthaven Manager. They should be considered stable enough for use in
Frosthaven Manager. Changes to an internally-used API should be made with care
and compelling reason.

@section{@tt{defns}}
@defmodule[frosthaven-manager/defns]

@subsection{Level Info}

@defthing[number-of-levels natural-number/c]{
A constant representing the number of possible levels, as opposed to what the
levels are.
}

@subsection{Scenario}

@defproc[(initiative? [v any/c]) boolean?]{
A predicate recognizing valid initiative values.
}

@subsection{Monster Cards}

@defstruct*[monster-info
             ([set-name string?]
              [name string?]
              [normal-stats (apply list/c (build-list number-of-levels (const monster-stats?)))]
              [elite-stats (apply list/c (build-list number-of-levels (const monster-stats?)))])]{
The monster information representation, often for reading pre-fab structs.
}

@defstruct*[monster-stats
             ([max-hp positive-integer?]
              [move natural-number/c]
              [attack natural-number/c]
              [bonuses (listof string?)]
              [effects (listof string?)]
              [immunities (listof string?)])]{
The monster statistic representation, usually used with pre-fabs.
}

@defstruct*[monster-action
             ([set-name string?]
              [name string?]
              [initiative initiative?]
              [abilities (listof string?)]
              [shuffle? boolean?])]{
The monster action representation, often for reading pre-fab structs.
}

@section{@tt{elements}}
@defmodule[frosthaven-manager/elements]

@section{@tt{enum-helpers}}
@defmodule[frosthaven-manager/enum-helpers]

@section{@tt{gui/common-menu}}
@defmodule[frosthaven-manager/gui/common-menu]

@section{@tt{gui/counter}}
@defmodule[frosthaven-manager/gui/counter]

@section{@tt{gui/elements}}
@defmodule[frosthaven-manager/gui/elements]

@section{@tt{gui/hierlist}}
@defmodule[frosthaven-manager/gui/hierlist]

@section{@tt{gui/level-info}}
@defmodule[frosthaven-manager/gui/level-info]

@section{@tt{gui/loot-picker}}
@defmodule[frosthaven-manager/gui/loot-picker]

@section{@tt{gui/markdown}}
@defmodule[frosthaven-manager/gui/markdown]

@section{@tt{gui/mixins}}
@defmodule[frosthaven-manager/gui/mixins]

@section{@tt{gui/monsters}}
@defmodule[frosthaven-manager/gui/monsters]

@section{@tt{gui/player-info}}
@defmodule[frosthaven-manager/gui/player-info]

@section{@tt{gui/start}}
@defmodule[frosthaven-manager/gui/start]

@section{@tt{gui/static-table}}
@defmodule[frosthaven-manager/gui/static-table]

@section{@tt{manager}}
@defmodule[frosthaven-manager/manager]

@section{@tt{monster-db}}
@defmodule[frosthaven-manager/monster-db]

@section{@tt{observable-operator}}
@defmodule[frosthaven-manager/observable-operator]

@section{@tt{qi}}
@defmodule[frosthaven-manager/qi]

