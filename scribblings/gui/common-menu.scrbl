#lang scribble/manual

@(require (for-label racket
                     racket/gui/easy
                     frosthaven-manager/gui/common-menu))

@title{@tt{gui/common-menu}}
@defmodule[frosthaven-manager/gui/common-menu]

@deftogether[(
    @defthing[about-menu-item (-> (is-a?/c view<%>))]
    @defthing[issue-menu-item (-> (is-a?/c view<%>))]
    @defthing[feature-menu-item (-> (is-a?/c view<%>))]
    @defthing[contribute-menu-item (-> (is-a?/c view<%>))]
    @defthing[send-feedback-menu-item (-> (is-a?/c view<%>))]
    @defthing[how-to-play-menu-item (-> (is-a?/c view<%>))]
    @defthing[launch-server-menu-item (-> (is-a?/c view<%>))]
    @defthing[gc-menu-item (-> (is-a?/c view<%>))]
)]{
Menu items for Frosthaven Manager.
}

@defproc[(do-about) renderer?]{
Renders an About window, as in @racket[about-menu-item]. Useful with
@racket[application-about-handler].
}

@defproc[(logs-widget [|@error-logs| (obs/c (or/c #f path?))])
         (is-a?/c view<%>)]{
A GUI view to tell the user where logs can be found.
}
