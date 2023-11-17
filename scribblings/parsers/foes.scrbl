#lang scribble/manual

@(require (for-label (except-in racket null)
                     megaparsack
                     frosthaven-manager/defns
                     frosthaven-manager/parsers/foes))

@title{@tt{parsers/foes}}
@defmodule[frosthaven-manager/parsers/foes]

This module contains parsers for @(hash-lang)
@racketmodname[frosthaven-manager/foes]. See
@secref{Programming_a_Scenario} for more details.

@defproc[(parse-foes [src any/c] [in input-port?] [#:syntax? syn? any/c])
         (or/c syntax? foes/pc)]{
The result is @racket[syntax?] with source @racket[src] if @racket[syn?] is
true, and the datum it contains matches @racket[foes/pc].
}

@deftogether[(@defthing[foes/pc flat-contract? #:value (listof (or/c (list/c 'import string?) monster-info? (listof monster-ability?) foe/pc))]
              @defthing[foe/pc flat-contract? #:value (list/c string? string? numbering/pc (listof spec/pc))]
              @defthing[spec/pc flat-contract? #:value (hash/c num-players/c monster-type/pc #:immutable #t)]
              @defthing[numbering/pc flat-contract? #:value (or/c "ordered" "random" #f)]
              @defthing[monster-type/pc flat-contract? #:value (or/c "absent" "normal" "elite")])]{
Contracts for foes values.
}

@deftogether[(@defthing[foes/p (parser/c char? foes/pc)]
              @defthing[foe/p (parser/c char? foe/pc)])]{
Textual parsers for parts of the foes language.
}
