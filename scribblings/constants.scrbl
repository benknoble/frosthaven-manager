#lang scribble/manual

@(require (for-label frosthaven-manager/constants))

@title{Constant Formatting and Parsing}
@defmodule[frosthaven-manager/constants]

This module provides forms for binding formatters and parsers where the mapping
from constant to string and vice-versa is static.

@defform[(define-constant-format/parse
           formatter-id parser-id
           ([constant-id string] ...))]{
Combines @racket[define-constant-format] and @racket[define-constant-parse].
}

@defform[(define-constant-format formatter-id ([constant-id string] ...))]{
Binds @racket[_formatter-id] to a function accepting constants @racket[_constant-id]
and producing the corresponding strings.
}

@defform[(define-constant-parse parser-id ([constant-id string] ...))]{
Binds @racket[_parser-id] to a function accepting strings and producing the
corresponding @racket[_constant-id]s.
}
