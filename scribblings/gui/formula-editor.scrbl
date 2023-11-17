#lang scribble/manual

@(require (for-label racket
                     racket/gui/easy
                     frosthaven-manager/parsers/formula))

@title{@tt{gui/formula-editor}}
@defmodule[frosthaven-manager/gui/formula-editor]

This module provides GUI objects for interactive formula editing.

@defproc[(formula-editor [|@|env env/c]) (is-a?/c view<%>)]{
A window containing an interactive formula editor.
}

@defproc[(formula-menu-item [|@|env env/c]) (is-a?/c view<%>)]{
A menu item that displays an interactive formula editor.
}
