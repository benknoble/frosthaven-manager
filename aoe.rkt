#lang racket

(provide (rename-out [mb #%module-begin]) #%app #%datum #%top #%top-interaction)

(require syntax/parse/define
         (for-syntax racket/syntax)
         frosthaven-manager/aoe-images)

(define-syntax-parser mb
  [(_ spec:expr)
   #:with aoe (format-id this-syntax "aoe" #:source this-syntax)
   (syntax/loc this-syntax
     (#%module-begin
      (provide aoe)
      (define (aoe)
        (spec->shape 'spec))
      (module+ main
        (require racket/gui pict)
        (show-pict (aoe)))))])

(module reader syntax/module-reader
  frosthaven-manager/aoe
  #:module-wrapper
  (λ (make-module stx?)
    (cond
      [stx? (syntax-parse (make-module)
              [(module name module-path
                 (#%module-begin x:aoe-spec ...))
               (define xs (syntax->list #'(x ...)))
               (define spec (datum->syntax #'(x ...) (syntaxes->spec xs) #'(x ...)))
               (strip-context
                 #`(module name module-path
                     (#%module-begin #,spec)))])]
      [else (make-module)]))
  (require syntax/parse
           syntax/strip-context
           frosthaven-manager/aoe-images)
  (define-syntax-class aoe-spec
    [pattern {~or {~datum s} {~datum x} {~datum o} {~datum m} {~datum g}}]))
