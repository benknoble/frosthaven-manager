#lang scribble/manual

@(require (for-label racket
                     racket/gui/easy
                     racket/gui/easy/contract
                     frosthaven-manager/manager
                     frosthaven-manager/gui/render))

@title{@tt{gui/manager}}
@defmodule[frosthaven-manager/gui/manager]

This module's main function is to run the Frosthaven Manager. It provides only
a single binding:

@defproc[(manager [s state?]) (is-a?/c window-view<%>)]{
A view for the Frosthaven Manager. Render with @racket[render/eventspace].
}
