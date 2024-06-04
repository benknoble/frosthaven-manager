#lang scribble/manual

@(require (for-label racket
                     racket/serialize
                     rebellion/type/enum
                     frosthaven-manager/enum-helpers))

@title{@tt{enum-helpers}}
@defmodule[frosthaven-manager/enum-helpers]

@defform[(define-serializable-enum-type id (constant-id ...) enum-option ...)
         #:grammar ([enum-option #:omit-root-binding
                                 (code:line #:descriptor-name descriptor-id)
                                 (code:line #:predicate-name predicate-id)
                                 (code:line #:discriminator-name discriminator-id)
                                 (code:line #:selector-name selector-id)
                                 (code:line #:property-maker prop-maker-expr)
                                 (code:line #:inspector inspector-expr)])
         #:contracts ([prop-maker-expr (-> uninitialized-enum-descriptor?
                                           (listof (cons/c struct-type-property? any/c)))]
                      [inspector-expr inspector?])]{
Exactly like @racket[define-enum-type], but with the addition of
@racket[prop:serializable] via a deserialize-info named
@racketidfont{deserialize-info:}@racket[id].
}
