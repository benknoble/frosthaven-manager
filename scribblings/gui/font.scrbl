#lang scribble/manual

@(require (for-label racket/gui))

@title{@tt{gui/font}}
@defmodule[frosthaven-manager/gui/font]

This module provides helpers for manipulating font objects, as in
@racket[font%].

@defproc[(copy-font
           [f (is-a?/c font%)]
           [#:size size (real-in 0.0 1024.0) (send f get-size size-in-pixels?)]
           [#:face face (or/c string? #f) (send f get-face)]
           [#:family family (or/c 'default 'decorative 'roman 'script 'swiss 'modern 'symbol 'system)
            (send f get-family)]
           [#:style style (or/c 'normal 'italic 'slant) (send f get-style)]
           [#:weight weight font-weight/c (send f get-weight)]
           [#:underlined? underlined? any/c (send f get-underlined)]
           [#:smoothing smoothing (or/c 'default 'partly-smoothed 'smoothed 'unsmoothed)
            (send f get-smoothing)]
           [#:size-in-pixels? size-in-pixels? any/c (send f get-size-in-pixels)]
           [#:hinting hinting (or/c 'aligned 'unaligned) (send f get-hinting)]
           [#:feature-settings feature-settings font-feature-settings/c
            (send f get-feature-settings)]
           [#:font-list font-list (or/c (is-a?/c font-list%) #f) (current-font-list)])
         (is-a?/c font%)]{
Copy all the features of font @racket[f] to a brand new font object. Supply
modified values via the keyword arguments.
}

@defthing[big-control-font (is-a?/c font%)]{
A font bigger than @racket[normal-control-font] and italic, but otherwise the
same.
}
