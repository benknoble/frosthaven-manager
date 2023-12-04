#lang scribble/manual

@(require (for-label racket
                     racket/gui/easy
                     racket/gui/easy/contract
                     frosthaven-manager/elements
                     frosthaven-manager/manager))

@title{@tt{gui/elements}}
@defmodule[frosthaven-manager/gui/elements]

@defproc[(elements-cycler
           [|@|states (listof (obs/c element-state/c))]
           [es (listof element-pics?)]
           [panel (unconstrained-domain-> (is-a?/c view<%>)) hpanel])
         (is-a?/c view<%>)]{
Returns a GUI view displaying the @racket[element-pics]. Each element of
@racket[es] is controlled by the corresponding element of @racket[|@|states].
}
