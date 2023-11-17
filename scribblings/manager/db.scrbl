#lang scribble/manual

@(require (for-label racket
                     frosthaven-manager/manager))

@title{@tt{manager/db}}
@defmodule[frosthaven-manager/manager/db]

This module provides facilities for manipulating the active monster databases.

@defproc[(init-dbs [db path-string?] [s state?]) any]{
Initialize the active monster databases.
}

@defproc[(init-dbs-and-foes [db path-string?] [s state?]) any]{
Initialize the active monster databases, exactly as @racket[init-dbs].
Additionally, initialize the foes from @racket[db] if it provides a foes
specification. This manipulates @racket[(state-@creatures s)]; see also
@racket[add-or-remove-monster-group] and
@racketmodname[frosthaven-manager/foes].
}
