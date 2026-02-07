#lang racket

(provide (rename-out [mb #%module-begin]) #%app #%datum #%top #%top-interaction)

(require frosthaven-manager/aoe-images
         syntax/parse/define)

(define-syntax-parse-rule (mb spec:expr)
  (syntax/loc this-syntax
    (#%module-begin (provide aoe)
                    (define (aoe)
                      (spec->shape 'spec))
                    (module+ main
                      (require racket/gui
                               pict)
                      (show-pict (aoe))))))

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
  (require frosthaven-manager/aoe-images
           syntax/parse
           syntax/strip-context)
  (define-syntax-class aoe-spec
    [pattern {~or {~datum s} {~datum x} {~datum o} {~datum m} {~datum g}}]))
