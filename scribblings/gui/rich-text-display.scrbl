#lang scribble/manual

@(require (for-label (except-in racket/gui newline)
                     racket/gui/easy
                     racket/gui/easy/contract
                     (only-in pict pict?)
                     frosthaven-manager/gui/rich-text-display))

@title{@tt{gui/rich-text-display}}
@defmodule[frosthaven-manager/gui/rich-text-display]

This module provides a view-based rich text display, suitable for certain
replacements of @racket[text].

@defproc[(rich-text-display
           [|@|content (maybe-obs/c (listof (or/c string?
                                                  pict?
                                                  pict/alt-text?
                                                  newline?)))]
           [#:font |@|font (maybe-obs/c (is-a?/c font%)) normal-control-font]
           [#:min-size |@|min-size (maybe-obs/c size/c) '(#f #f)]
           [#:stretch |@|stretch (maybe-obs/c stretch/c) '(#t #t)]
           [#:margin |@|margin (maybe-obs/c margin/c) '(0 0)]
           [#:inset |@|inset (maybe-obs/c margin/c) '(5 5)]
           [#:style style (listof (one-of/c 'no-border 'control-border 'combo
                                            'no-hscroll 'no-vscroll
                                            'hide-hscroll 'hide-vscroll
                                            'auto-vscroll 'auto-hscroll
                                            'resize-corner 'deleted 'transparent))
            '(no-hscroll)])
         (is-a?/c view<%>)]{
Displays @racket[|@|content] via a @racket[text%]. Keyword arguments except
@racket[|@|font] control the @racket[text%]. The @racket[|@|font] controls the
font at which text is displayed and to which pictures are scaled.

Contents are selectable and copyable with the usual keyboard shortcuts, and can
also be selected with mouse. Contents are automatically reflowed.
}

@defthing[newline newline?]{
When used with @racket[rich-text-display], forces a hard newline.
}

@defproc[(pict/alt-text [p pict?] [alt-text string?])
         pict/alt-text?]{
When used with @racket[rich-text-display], displays a @racket[pict?]. Copying
the pict copies the @racket[alt-text] instead.
}

@defproc[(newline? [x any/c]) boolean?]{
A predicate that recognizes the @racket[newline] value.
}

@defproc[(pict/alt-text? [x any/c]) boolean?]{
A predicate that recognizes values produced by @racket[pict/alt-text].
}
