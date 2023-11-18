#lang scribble/manual

@(require (for-label (except-in racket null)
                     frosthaven-manager/defns
                     frosthaven-manager/parsers/formula))

@title{@tt{defns}}
@defmodule[frosthaven-manager/defns]

This module reprovides everything from
@racketmodname[frosthaven-manager/defns/level],
@racketmodname[frosthaven-manager/defns/loot],
@racketmodname[frosthaven-manager/defns/monsters],
@racketmodname[frosthaven-manager/defns/players], and
@racketmodname[frosthaven-manager/defns/scenario].

@include-section{defns/level.scrbl}
@include-section{defns/loot.scrbl}
@include-section{defns/monsters.scrbl}
@include-section{defns/players.scrbl}
@include-section{defns/scenario.scrbl}
