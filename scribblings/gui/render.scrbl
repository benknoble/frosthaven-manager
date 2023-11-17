#lang scribble/manual

@(require "../common.rkt")
@(require (for-label racket/gui
                     racket/gui/easy
                     racket/gui/easy/contract
                     frosthaven-manager/gui/render))

@title{@tt{gui/render}}
@defmodule[frosthaven-manager/gui/render]

@defparam[current-renderer
           r (or/c #f renderer?)
           #:value #f]{
A parameter for the current renderer. This can be set so that sub-views can
access the top-level renderer. Note that it is not re-entrant, in the sense that
to make it effective one must render an application by
@codeblock{
(define root (render ...))
(current-renderer root)
}
Any other application running in the same thread cannot use
@racket[current-renderer] or it will interfere with the previous application.
This also holds more generally of sub-views @racket[render]ed on-the-fly. See
@racket[render/eventspace] to avoid this.

This will not affect multiple applications built and run separately that use
this library, since they're in separate processes completely.
}

@defproc[(render/eventspace [tree (is-a?/c view<%>)]
                            [#:parent parent (or/c #f renderer?) #f]
                            [#:eventspace es eventspace? (current-eventspace)])
         renderer?]{
Renders (as in @racket[render]) @racket[tree] with parent @racket[parent] in the
eventspace @racket[es], then queues a high-priority callback in the
handler-thread for @racket[es] to set @racket[current-renderer] to the resulting
renderer, which is returned.

Pass a new @tech[#:doc gui-doc]{eventspace} created with
@racket[make-eventspace] to separate the rendered @racket[tree] and
corresponding @racket[current-renderer] from other applications.

This can be used to group windows in an application together, but note that
subsequent calls with the same @racket[es] will override that eventspace's
@tech[#:doc gui-doc]{handler thread}'s @racket[current-renderer].

For a short-lived window that should tear down the eventspace on closure,
combine with @racket[with-closing-custodian/eventspace].
}

@deftogether[(
             @defform[(with-closing-custodian/eventspace e ...+)]
             @defform[#:id closing-custodian closing-custodian]
             @defform[#:id closing-eventspace closing-eventspace]
             @defform[#:id close-custodian-mixin close-custodian-mixin]
             )]{
Evaluates the body expressions @racket[e ...] with the following special
variables available:
@itemize[
         @item{@racket[closing-custodian] is a new @tech[#:doc ref-doc]{custodian} that manages @racket[closing-eventspace].}
         @item{@racket[closing-eventspace] is a new @tech[#:doc gui-doc]{eventspace} managed by @racket[closing-custodian].}
         @item{@racket[close-custodian-mixin] is a new @tech[#:doc ref-doc]{mixin} for @racket[top-level-window<%>]s that causes @racket[closing-custodian] to shutdown after the corresponding window is closed.}
         ]
For example, the following produces either @racket[#t] or @racket[#f] depending
on whether window A or window B was closed first. Note also the use of
@racket[render/eventspace] to set @racket[current-renderer] correctly.
@codeblock{
(require racket/gui/easy)
(define main-es (make-eventspace))
(render/eventspace #:eventspace main-es (window #:title "A" (text "A")))
(define aux-es
  (with-closing-custodian/eventspace
    (render/eventspace
      #:eventspace closing-eventspace
      (window #:mixin close-custodian-mixin
              #:title "B"
              (text "B")))
    closing-eventspace))

(sync main-es) ;; wait until window A is closed
(eventspace-shutdown? aux-es) ;; true if window B was closed first
}
}
