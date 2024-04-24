#lang scribble/manual

@(require (for-label racket
                     racket/gui/easy
                     (except-in racket/gui/easy/operator
                                ~>
                                <~)
                     racket/gui/easy/contract
                     qi))

@title{@tt{observable-operator}}
@defmodule[frosthaven-manager/observable-operator]

In addition to the shorthands below, this module exports @racket[define/obs],
@racket[|@|], @racket[:=], and @racket[λ:=] from
@racketmodname[racket/gui/easy/operator] and everything from @racketmodname[qi]
via @racketmodname[frosthaven-manager/curlique].

@defproc[(|<@| [|@o| obs?] [f (-> any/c any/c)]) any/c]{
An alias for @racket[obs-update!].
}

@defproc[(|@>| [|@o| obs?] [f (-> any/c any/c)]) obs?]{
An alias for @racket[obs-map].
}

@defproc[(|λ<@| [|@o| obs?] [f (-> any/c any/c)]) (-> any/c)]{
An alias for @racket[λ<~].
}

@defproc[(|@!| [|@o| obs?]) any/c]{
An alias for @racket[obs-peek].
}
