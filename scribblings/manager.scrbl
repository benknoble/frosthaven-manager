#lang scribble/manual

@(require (for-label racket
                     frosthaven-manager/manager))

@title{@tt{manager}}
@defmodule[frosthaven-manager/manager]

This module reprovides all the bindings from
@racketmodname[frosthaven-manager/manager/state],
@racketmodname[frosthaven-manager/manager/ability-decks],
@racketmodname[frosthaven-manager/manager/modifier-decks],
@racketmodname[frosthaven-manager/manager/db],
@racketmodname[frosthaven-manager/manager/loot], and
@racketmodname[frosthaven-manager/manager/round-prompts].

@include-section{manager/state.scrbl}
@include-section{manager/ability-decks.scrbl}
@include-section{manager/modifier-decks.scrbl}
@include-section{manager/db.scrbl}
@include-section{manager/loot.scrbl}
@include-section{manager/round-prompts.scrbl}
