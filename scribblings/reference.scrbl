#lang scribble/manual

@(require "common.rkt"
          (for-label
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

