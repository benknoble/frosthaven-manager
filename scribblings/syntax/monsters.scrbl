#lang scribble/manual

@(require (for-label (except-in racket null)
                     frosthaven-manager/defns
                     frosthaven-manager/monster-db
                     frosthaven-manager/parsers/foes
                     frosthaven-manager/syntax/monsters))

@title{@tt{syntax/monsters}}
@defmodule[frosthaven-manager/syntax/monsters]

@defform[#:literals (provide import info ability)
         (make-dbs (provide info-db-id ability-db-id)
                   (import import-mod-path ...)
                   (info monster-info ...)
                   (ability monster-ability ...))
         #:contracts ([monster-info monster-info?]
                      [monster-ability monster-ability?])]{
Binds and provides @racket[info-db-id] and @racket[ability-db-id] to
@racket[info-db/c] and @racket[ability-db/c] values, respectively, by importing
all the monster information from each @racket[import-mod-path] and merging it
with the provided @racket[monster-info] and @racket[monster-ability].

Each @racket[import-mod-path] is expected to provide the same
@racket[info-db-id] and @racket[ability-db-id].

The @racket[provide] keyword in the provide specification is recognized by
binding and must be the same as the one from @racketmodname[racket/base]. The
@racket[import], @racket[info], and @racket[ability] keywords are recognized by
datum identity.
}

@defproc[(imports->dbs [import-paths (listof string?)])
         (values (listof info-db/c) (listof ability-db/c))]{
Produces all the monster information databases, one for each import in
@racket[import-paths], using @racket[get-dbs].
}

@defproc[(check-monsters-have-abilities
           [imported-info-dbs (listof info-db/c)]
           [imported-ability-dbs (listof ability-db/c)]
           [infos (listof monster-info?)]
           [actions (listof monster-ability?)])
         boolean?]{
True iff the set names among all the given @racket[imported-info-dbs] and
@racket[infos] is a subset of those among all the given
@racket[imported-ability-dbs] and @racket[actions].
}

@defproc[(check-monsters-have-abilities-message
           [imported-info-dbs (listof info-db/c)]
           [imported-ability-dbs (listof ability-db/c)]
           [infos (listof monster-info?)]
           [actions (listof monster-ability?)])
         string?]{
An error message for when @racket[check-monsters-have-abilities] fails.
}

@defproc[(check-foes-have-monsters
           [imported-info-dbs (listof info-db/c)]
           [infos (listof monster-info?)]
           [foes (listof foe/pc)])
         boolean?]{
True iff the foe names among all the given @racket[foes] is a subset of the
monster names among all the given @racket[imported-info-dbs] and @racket[infos].
}

@defproc[(check-foes-have-monsters-message
           [imported-info-dbs (listof info-db/c)]
           [infos (listof monster-info?)]
           [foes (listof foe/pc)])
         string?]{
An error message for when @racket[check-foes-have-monsters] fails.
}
