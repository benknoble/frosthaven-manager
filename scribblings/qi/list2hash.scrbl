#lang scribble/manual

@(require (for-label racket
                     qi
                     frosthaven-manager/qi/list2hash))

@title{@tt{qi/list2hash}}
@defmodule[frosthaven-manager/qi/list2hash]

@defform[(list~>hash maybe->key maybe->value)
          #:grammar
          [(maybe->key (code:line)
                       (code:line #:->key ->key-flo))
           (maybe->value (code:line)
                         (code:line #:->value ->value-flo))]]{
This Qi form transforms the input value (a list) into a hash, where each element
of the list is mapped into a key via @racket[->key-flo] and a value via
@racket[->value-flo]. It uses @racket[list->hash] as implementation.
}

@defproc[(list->hash [xs list?]
                     [#:->key ->key (-> any/c any/c) identity]
                     [#:->value ->value (-> any/c any/c) identity])
         hash?]{
Transforms @racket[xs] to a hash by mapping each element into a key via
@racket[->key] and a value via @racket[->value].
}
