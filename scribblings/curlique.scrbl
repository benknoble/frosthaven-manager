#lang scribble/manual

@(require (for-label racket
                     (prefix-in qi: qi)
                     (except-in frosthaven-manager/curlique #%app))
          scribble/example)

@(define my-eval
   (make-base-eval '(require racket frosthaven-manager/curlique)))

@title{@tt{curlique}}
@defmodule[frosthaven-manager/curlique]

This module provides a shorthand notation for Qi flows. It overrides Racket's
@racket[#%app]: forms written in curly braces like @racket[{(all positive?)}]
are implictly wrapped in @racket[flow] from Qi.

In addition, it provides the following overrides:

@deftogether[(@defidform[~>] @defidform[~>>] @defidform[switch])]{
Like equivalent forms from Qi, @racket[qi:~>], @racket[qi:~>>],
@racket[qi:switch], but if written with curly braces, they are implicitly
wrapped in @racket[flow] instead of acting on specified values.

@examples[#:eval my-eval #:hidden
  (define-syntax (CB stx)
    (syntax-case stx ()
      [(_ input ...)
       #'(eval (syntax-property #'(#%app input ...) 'paren-shape #\{ ))]))
  (define-syntax (CB-mac stx)
    (syntax-case stx ()
      [(_ input ...)
       #'(eval (syntax-property #'(input ...) 'paren-shape #\{ ))]))
]
@examples[#:eval my-eval
  (code:comment "A very small identity")
  (eval:alts (map {} (range 10))
             (map (CB) (range 10)))
  (eval:alts (define all-good? {(all positive?)})
             (define all-good? (CB (all positive?))))
  (all-good? 1 2 3 4)
  (all-good? 1 -2 3 4)
  (~> (1 2 3) (-< + count) /)
  (eval:alts (define average {~> (-< + count) /})
             (define average (CB-mac ~> (-< + count) /)))
  (average 1 2 3)
]
}

@(close-eval my-eval)
