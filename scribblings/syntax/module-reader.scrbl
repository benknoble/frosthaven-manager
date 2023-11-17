#lang scribble/manual

@(require (for-label racket
                     frosthaven-manager/parsers/monster
                     frosthaven-manager/parsers/foes
                     (only-in frosthaven-manager/syntax/module-reader)))

@title{@tt{syntax/module-reader}}
@defmodule[frosthaven-manager/syntax/module-reader]

This expander language wraps @racketmodname[syntax/module-reader] by assuming a
specific reading protocol.

This module does not have a reader of its own, so should be used with
@racket[module] or @(hash-lang) @racketmodname[s-exp].

@defform[#:literals (from)
         (#%module-begin expander-mod-path
          [parser-id from parser-mod-path])]{
The following example demonstrates the entire grammer of the expander language:
@codeblock|{
#lang s-exp frosthaven-manager/syntax/module-reader
frosthaven-manager/foes
[parse-foes from frosthaven-manager/parsers/foes]
}|

Or with @racket[module]:
@codeblock|{
#lang racket
(module reader frosthaven-manager/syntax/module-reader
  frosthaven-manager/foes
  [parse-foes from frosthaven-manager/parsers/foes])
}|

The semantics are as follows. The resulting module satisfies the language reader
extension protocol from @secref["parse-reader" #:doc '(lib "scribblings/reference/reference.scrbl")]
via @racketmodname[syntax/module-reader] with a few specifications. The
@racket[expander-mod-path] is used as in @racketmodname[syntax/module-reader] to
determine the module-path for the initial bindings of modules produced by the
reader. The @racket[parser-id], which must be provided by
@racket[parser-mod-path], is assumed to parse the whole body as with the
@racket[#:whole-body-readers?] keyword for @racketmodname[syntax/module-reader].
In addition, it should support the following protocol: the parser accepts 2
positional arguments. The first is the same name-value as @racket[read-syntax];
the second is the same input port as for @racket[read] and @racket[read-syntax]
with line-counting enabled. Then it must accept a keyword option
@racket[#:syntax?], whose value is a boolean indicating whether or not to
produce a syntax object.

Examples of valid parsers include @racket[parse-foes] and
@racket[parse-bestiary].
}
