#lang scribble/manual

@(require (for-label racket
                     racket/gui/easy
                     racket/gui/easy/contract
                     frosthaven-manager/gui/counter))

@title{@tt{gui/counter}}
@defmodule[frosthaven-manager/gui/counter]

@defproc[(counter [|@label| (maybe-obs/c string?)]
                  [up (-> any)]
                  [down (-> any)])
         (is-a?/c view<%>)]{
A GUI component for a counter with a label and up and down callbacks.
}
