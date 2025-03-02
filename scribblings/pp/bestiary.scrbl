#lang scribble/manual

@(require "../common.rkt")
@(require (for-label racket
                     (prefix-in pretty: pretty-expressive)
                     frosthaven-manager/parsers/monster
                     pict))

@title{@tt{pp/bestiary}}
@defmodule[frosthaven-manager/pp/bestiary]

This module pretty-prints bestiary files. It can be run as a program with

@terminal|{
racket -l- frosthaven-manager/pp/bestiary
}|

to format standard in or a provided file to standard out. Use @DFlag{help} for
more options.

@defproc[(pretty-bestiary [bestiary bestiary/c] [#:lang-line? lang-line? any/c #t]) pretty:doc?]{
Creates a document for pretty printing from the results of a parsed bestiary.
The document starts with a @(hash-lang) line preceding the result if
@racket[lang-line?] is not @racket[#f].

The @racket[bestiary] must not contain any @racket[pict] values, so it composes
best with @racket[parse-bestiary].
}
