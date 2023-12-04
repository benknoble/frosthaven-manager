#lang scribble/manual

@(require (for-label racket
                     racket/gui/easy/contract
                     frosthaven-manager/manager/elements))

@title{@tt{manager/elements}}
@defmodule[frosthaven-manager/manager/elements]

@defthing[element-state/c
           contract?
           #:value (or/c 'unfused 'infused 'waning)]{
A contract recognizing valid element states.
}

@defproc[(make-states [es (listof any/c)])
         (listof (obs/c element-state/c))]{
Builds an equally-sized list of element states to control @racket[es] in
@racket[elements-cycler].
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
