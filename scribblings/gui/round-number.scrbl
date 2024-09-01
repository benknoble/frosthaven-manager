#lang scribble/manual

@(require (for-label racket
                     racket/gui/easy
                     racket/gui/easy/contract
                     frosthaven-manager/gui/round-number))

@title{@tt{gui/round-number}}
@defmodule[frosthaven-manager/gui/round-number]

This module contains GUI components for interacting with the round number.

@defproc[(round-number-modifier [|@|round (obs/c natural-number/c)]
                                [#:new-round-number new-round-number
                                 (-> (-> natural-number/c natural-number/c) any)
                                 void])
         (is-a?/c window-view<%>)]{
This dialog, when rendered, provides buttons to set the displayed
@racket[|@|round]. The action passed up by @racket[new-round-number] is a
procedure that computes a new round value from the old one.
}
