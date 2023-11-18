#lang scribble/manual

@(require (for-label (except-in racket null)
                     frosthaven-manager/defns
                     frosthaven-manager/parsers/formula))

@title{Scenario}
@defmodule[frosthaven-manager/defns/scenario]

@deftogether[(
              @defthing[element? predicate/c]

              @defthing[fire element?]
              @defthing[ice element?]
              @defthing[air element?]
              @defthing[earth element?]
              @defthing[light element?]
              @defthing[dark element?]
)]{
The elements.

Serializable.
}

@deftogether[(
              @defthing[monster-modifier? predicate/c]

              @defthing[zero monster-modifier?]
              @defthing[minus1 monster-modifier?]
              @defthing[plus1 monster-modifier?]
              @defthing[minus2 monster-modifier?]
              @defthing[plus2 monster-modifier?]
              @defthing[null monster-modifier?]
              @defthing[crit monster-modifier?]
              @defthing[curse monster-modifier?]
              @defthing[bless monster-modifier?]
)]{
Monster modifier cards.

Serializable.
}

@deftogether[(
              @defthing[condition? predicate/c]

              @defthing[regenerate condition?]
              @defthing[ward condition?]
              @defthing[invisible condition?]
              @defthing[strengthen condition?]
              @defthing[wound condition?]
              @defthing[brittle condition?]
              @defthing[bane condition?]
              @defthing[poison condition?]
              @defthing[immobilize condition?]
              @defthing[disarm condition?]
              @defthing[impair condition?]
              @defthing[stun condition?]
              @defthing[muddle condition?]
)]{
The @racket[condition?] predicate recognizes all valid conditions, which are
listed here.

Serializable.
}

@deftogether[(
              @defproc[(discriminator:condition [c condition?]) integer?]
              @defproc[(selector:condition [i integer?]) condition?]
)]{
@tech[#:doc '(lib "rebellion/main.scrbl")]{Enum discriminator} and
@tech[#:doc '(lib "rebellion/main.scrbl")]{enum selector} for
@racket[condition?] values. Both contract error when the argument is outside the
appropriate domain.
}

@defproc[(initiative? [v any/c]) boolean?]{
A predicate recognizing valid initiative values.
}

@defthing[conditions (listof condition?)]{
All the conditions together.
}

@defthing[monster-modifier-deck (listof monster-modifier?)]{
A full deck of 20 monster modifier cards.
}

@deftogether[(
              @defthing[monster-curse-deck (listof monster-modifier?)]
              @defthing[bless-deck (listof monster-modifier?)]
)]{
Full decks of 10 monster curse and bless cards.
}

@defproc[(shuffle-modifier-deck? [deck (listof monster-modifier?)]) boolean?]{
True if-and-only-if @racket[deck] contains a @racket[null] or @racket[crit].
}

@deftogether[(
              @defproc[(better-modifier [a monster-modifier?]
                                        [b monster-modifier?])
                       monster-modifier?]
              @defproc[(worse-modifier [a monster-modifier?]
                                       [b monster-modifier?])
                       monster-modifier?]
)]{
Returns the better or worse of the two modifier cards.
}
