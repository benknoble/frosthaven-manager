#lang scribble/manual

@(require (for-label racket
                     pict
                     frosthaven-manager/elements))

@title{@tt{elements}}
@defmodule[frosthaven-manager/elements]

@defthing[size natural-number/c]{
The size of the element pictures.
}

@defstruct*[element-pics
             ([name string?]
              [infused pict?]
              [waning pict?]
              [unfused pict?]
              [consume pict?])
             #:transparent]{
A container for a named set of element pictures.
}

@defproc[(elements) (listof element-pics?)]{
Returns all of the elements bundled together. This module also provides bindings
from the names of the elemnts to procedures returning @racket[element-pics]
values, but they are not documented here. See @secref{Elements_Tracker} for the
various element names and pictures.
}
