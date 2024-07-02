#lang scribble/manual

@(require (for-label (except-in racket null)
                     racket/gui/easy
                     racket/gui/easy/contract
                     frosthaven-manager/defns))

@title{@tt{gui/level-picker}}
@defmodule[frosthaven-manager/gui/level-picker]

@defproc[(level-picker
           [#:choose on-choose (-> level/c any)]
           [#:selection selection (maybe-obs/c level/c)]
           [#:label label (maybe-obs/c maybe-label/c) #f])
         (is-a?/c view<%>)]{
A GUI view that presents a choice of Frosthaven level values; @racket[on-choose]
is invoked whenever the choice changes. The selection may be controlled with
@racket[selection]. The optional @racket[label] is used as with @racket[choice].
}
