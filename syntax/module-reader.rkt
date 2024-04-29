#lang racket

(provide
  #%app #%datum #%top #%top-interaction
  (rename-out [mb #%module-begin]))

(require syntax/parse/define
         (prefix-in syntax/module-reader: syntax/module-reader))

(define-syntax-parser mb
  [(_ expander [parser:id {~datum from} parser-mod])
   (syntax/loc this-syntax
     (syntax/module-reader:#%module-begin
      expander
      #:whole-body-readers? #t
      #:read-syntax read-syntax
      #:read read
      (require parser-mod)
      (define read-syntax (make-read-syntax parser))
      (define read (make-read parser))))])

(define ((make-read-syntax parser) src in)
  (port-count-lines! in)
  (parser src in #:syntax? #t))

(define ((make-read parser) in)
  (port-count-lines! in)
  (parser (object-name in) in #:syntax? #f))
