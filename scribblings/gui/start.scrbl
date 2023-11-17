#lang scribble/manual

@(require (for-label (except-in racket null)
                     frosthaven-manager/defns
                     racket/gui/easy))

@title{@tt{gui/start}}
@defmodule[frosthaven-manager/gui/start]

@defproc[(start-view
           [#:on-level on-level (-> level/c any) void]
           [#:on-player on-player (-> num-players/c any) void])
         (is-a?/c view<%>)]{
A GUI view for the start screen of Frosthaven Manager. The callbacks are invoked
with the level and number of players for each update to those values.
}
