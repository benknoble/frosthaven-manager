#lang scribble/manual

@(require (for-label racket/gui/easy
                     frosthaven-manager/manager
                     (prefix-in server: frosthaven-manager/server)
                     frosthaven-manager/gui/render))

@title{@tt{gui/server}}
@defmodule[frosthaven-manager/gui/server]

@defproc[(launch-server [s state?]) renderer?]{
Renders a window in a new closing eventspace with server information, and
launches a server. See @racket[with-closing-custodian/eventspace] and
@racket[server:launch-server].
}
