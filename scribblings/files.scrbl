#lang scribble/manual

@(require (for-label racket/gui
                     frosthaven-manager/files))

@title{@tt{files}}
@defmodule[frosthaven-manager/files]

@defproc[(get-file/filter [message string?] [filter (list/c string? string?)])
         (or/c path? #f)]{
Returns @racket[get-file] with @racket[message] and @racket[filter].
Additionally permits an "Any" filter. The Windows extensions is provided from
@racket[(second filter)].
}

@defproc[(put-file/filter [message string?] [filter (list/c string? string?)]
                          [directory path-string? #f]
                          [file path-string? #f])
         (or/c path? #f)]{
Returns @racket[put-file] with @racket[message] and @racket[filter].
Additionally permits an "Any" filter. The Windows extensions is provided from
@racket[(second filter)].
}
