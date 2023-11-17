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

@deftogether[(@defproc[(infuse-all [es (listof (obs/c element-state/c))]) any]
              @defproc[(consume-all [es (listof (obs/c element-state/c))]) any])]{
Set all element states @racket[es] to @racket['infused] or @racket['unfused],
respectively.
}

@defproc[(wane-element [state element-state/c])
         element-state/c]{
Returns the new element state after waning for one cycle.
}

@defproc[(transition-element-state [state element-state/c]) element-state/c]{
Returns the new element state after cycling once, with unfused wrapping around
to infused.
}
