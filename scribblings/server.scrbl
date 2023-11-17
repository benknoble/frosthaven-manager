#lang scribble/manual

@(require (for-label racket
                     frosthaven-manager/manager
                     frosthaven-manager/server))

@title{@tt{server}}
@defmodule[frosthaven-manager/server]

@defproc[(launch-server [s state?] [send-event procedure?])
         (values string? (-> any))]{
Launches the actual web server for @racket[s]. The callback protocol for
@racket[send-event] is not yet formalized and very unstable.

Returns the server address (on a best-guess basis) and a @code{stop} procedure
that stops the server when called.
}
