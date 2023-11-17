#lang scribble/manual

@(require "../common.rkt")
@(require (for-label xml
                     racket
                     racket/gui/easy
                     racket/gui/easy/contract
                     frosthaven-manager/gui/markdown))

@title{@tt{gui/markdown}}
@defmodule[frosthaven-manager/gui/markdown]

@defproc[(markdown-text
           [|@content| (maybe-obs/c (or/c string? path?))]
           [#:min-size @min-size (maybe-obs/c size/c) '(#f #f)]
           [#:stretch @stretch (maybe-obs/c stretch/c) '(#t #t)]
           [#:margin @margin (maybe-obs/c margin/c) '(0 0)]
           [#:inset @inset (maybe-obs/c margin/c) '(5 5)]
           [#:style style
            (listof (one-of/c 'no-border 'control-border 'combo
                              'no-hscroll 'no-vscroll
                              'hide-hscroll 'hide-vscroll
                              'auto-vscroll 'auto-hscroll
                              'resize-corner 'deleted 'transparent))
            '(no-hscroll)])
         (is-a?/c view<%>)]{
A GUI view rendering the markdown in @racket[|@content|], which is either a
@tech[#:doc ref-doc]{string} of Markdown or a path to a file containing Markdown.
The view updates when @racket[|@content|] does---note that in the string case
this means the Markdown content has changed, but in the path case this means the
path has changed, not the contents of the file at the path!

The following Markdown features are supported:
@itemlist[
          @item{Paragraphs;}
          @item{HTML comments;}
          @item{Hyperlinks;}
          @item{Blockquotes;}
          @item{Unordered and ordered lists;}
          @item{Horizontal rules;}
          @item{@bold{Bold}, @italic{italic}, and @tt{code} styles;}
          @item{and six levels of headings.}
]

The following @racket[xexpr?]s are supported recursively in the parsed Markdown;
these map to the Markdown features above:
@itemlist[
          @item{Any @tech[#:doc ref-doc]{string}}
          @item{Any expression tagged @tt{!HTML-COMMENT}, the tag for HTML comments}
          @item{Any expression tagged @tt{a}}
          @item{Any expression tagged @tt{blockquote}}
          @item{Any expression tagged @tt{ul}}
          @item{Any expression tagged @tt{ol}}
          @item{Any expression tagged @tt{li}}
          @item{Any expression tagged @tt{hr}}
          @item{Any expression tagged @tt{p}}
          @item{Any expression tagged @tt{strong}}
          @item{Any expression tagged @tt{em}}
          @item{Any expression tagged @tt{code}}
          @item{Any expression tagged @tt{h1}}
          @item{Any expression tagged @tt{h2}}
          @item{Any expression tagged @tt{h3}}
          @item{Any expression tagged @tt{h4}}
          @item{Any expression tagged @tt{h5}}
          @item{Any expression tagged @tt{h6}}
]
Any other tag found in the parsed Markdown is a runtime error.

Note that Markdown technically requires 4 spaces or a single tab as leading
indent for nesting lists and other blocks; while many Markdown implementations
(such as those used on GitHub) are more lenient, the implementation backing
@racket[markdown-text] is stricter on this point.
}
