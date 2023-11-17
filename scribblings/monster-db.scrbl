#lang scribble/manual

@(require (for-label racket
                     frosthaven-manager/monster-db))

@title{@tt{monster-db}}
@defmodule[frosthaven-manager/monster-db]

See @secref{Programming_a_Scenario} for more information on custom monster
databases.

@deftogether[(
              @defthing[info-db/c contract?]
              @defthing[ability-db/c contract?]
)]{
Contracts recognizing monster databases of @racket[monster-info] and
@racket[monster-ability] values.
}

@defproc[(datums->dbs [xs (listof any/c)])
         (values info-db/c ability-db/c)]{
Filters the @racket[monster-info] and @racket[monster-ability] values out of
@racket[xs] and produces monster databases.
}

@defproc[(get-dbs [db-file path-string?])
         (values info-db/c ability-db/c)]{
Reads @racket[db-file] and produces the monster databases.
}

@defthing[default-monster-db path-string?]{
The demo, default monster database included with Frosthaven Manager.
}
