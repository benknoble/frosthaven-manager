#lang scribble/manual

@(require (for-label pict
                     frosthaven-manager/icons)
          frosthaven-manager/icons)

@title{@tt{icons}}
@defmodule[frosthaven-manager/icons]

This module provides various icons that are spliced into ability card texts.
All replacements are case insensitive; any numbers or other accompanying text
are preserved. The signifier @racket[_N] denotes where a number is expected.

@(require syntax/parse/define)

@(define-syntax-parse-rule (deficon proc:id replacement:expr ...)
   @defproc[(proc) pict?]{@para[(proc)] Provides replacements for @itemlist[@item{@replacement} ...]})

@deficon[target "Target N" "Target all" "+N target(s)"]
@deficon[range "Range N"]
@deficon[push "Push N"]
@deficon[pull "Pull N"]
