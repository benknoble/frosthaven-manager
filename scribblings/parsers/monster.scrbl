#lang scribble/manual

@(require (for-label (except-in racket null)
                     frosthaven-manager/defns
                     megaparsack
                     frosthaven-manager/parsers/monster))

@title{@tt{parsers/monster}}
@defmodule[frosthaven-manager/parsers/monster]

This module contains parsers for @(hash-lang)
@racketmodname[frosthaven-manager/bestiary]. See
@secref{Programming_a_Scenario} for more details.

@defproc[(parse-bestiary [src any/c] [in input-port?] [#:syntax? syn? any/c])
         (or/c syntax? bestiary/c)]{
The result is @racket[syntax?] with source @racket[src] if @racket[syn?] is
true, and the datum it contains matches @racket[bestiary/c].
}

@defthing[bestiary/c flat-contract?
                     #:value
                     (list/c (cons/c 'import (listof string?))
                             (cons/c 'info (listof monster-info?))
                             (cons/c 'ability (listof monster-ability?)))]{
A contract for bestiary values.
}

@deftogether[(@defthing[monster/p (parser/c char? monster-info?)]
              @defthing[ability-deck/p (parser/c char? (listof monster-ability?))]
              @defthing[import-monsters/p (parser/c char? (list/c 'import string?))]
              @defthing[bestiary/p (parser/c char? bestiary/c)])]{
Textual parsers for parts of the bestiary language.
}

@defproc[(bestiary-dupes [xs (listof any/c)])
         (values (or/c #f (listof string?))
                 (or/c #f (listof string?)))]{
Returns duplicate monster names from bestiaries and ability decks in
@racket[xs]. The first value is based on any @racket[monster-info]s and the
second on @racket[monster-ability] decks.
}
