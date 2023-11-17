#lang scribble/manual

@(require (for-label (except-in racket null)
                     racket/gui/easy
                     racket/gui/easy/contract
                     frosthaven-manager/defns))

@title{@tt{gui/level-info}}
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
