#lang scribble/manual

@(require (for-label (except-in racket null)
                     frosthaven-manager/defns
                     racket/gui/easy
                     racket/gui/easy/contract
                     frosthaven-manager/manager
                     frosthaven-manager/gui/round-prompts))

@title{@tt{gui/round-prompts}}
@defmodule[frosthaven-manager/gui/round-prompts]

This module contains GUI utilities for
@racketmodname[frosthaven-manager/manager/round-prompts].

@defproc[(prompts-input-view [|@|prompts (obs/c (listof prompt/c))]
                             [#:on-add on-add (-> prompt/c any) void]
                             [#:on-remove on-remove (-> natural-number/c prompt/c any) void])
         (is-a?/c view<%>)]{
Views and constructs a list of round prompt values. The @racket[on-add] event is
emitted when a round prompt is added, and @racket[on-remove] when a round prompt
is removed. The @racket[on-remove] event signals both the index of the round
prompt and the prompt value to be removed.
}

@defproc[(manage-prompt-menu-item [|@|prompts (obs/c (listof prompt/c))]
                                  [#:on-add on-add (-> prompt/c any) void]
                                  [#:on-remove on-remove (-> natural-number/c prompt/c any) void])
         (is-a?/c view<%>)]{
Renders a dialog for managing round prompts in the same style as
@racket[prompts-input-view].
}

@defproc[(do-round-prompt [t time/c] [round natural-number/c]) any]{
Renders a dialog prompting players to check the rules based on the timing of the
current round.
}
