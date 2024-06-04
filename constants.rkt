#lang racket

(provide define-constant-format/parse
         define-constant-format
         define-constant-parse)

(require syntax/parse/define)

(begin-for-syntax
 (define-syntax-class clause
   #:attributes (constant string)
   [pattern [constant:id string:string]]))

(define-syntax-parser define-constant-format/parse
  [(_ formatter:id parser:id (c:clause ...))
   (syntax/loc this-syntax
     (begin
       (define-constant-format formatter (c ...))
       (define-constant-parse parser (c ...))))])

(define-syntax-parser define-constant-format
  [(_ formatter:id (c:clause ...))
   (syntax/loc this-syntax
     (define formatter
       (let ([table (hash {~@ c.constant c.string} ...)])
         (λ (x)
           (hash-ref table x (λ ()
                               (raise-arguments-error 'formatter
                                                      "value not eligible for formatting"
                                                      "value" x)))))))])

(define-syntax-parser define-constant-parse
  [(_ parser:id (c:clause ...))
   (syntax/loc this-syntax
     (define parser
       (let ([table (hash {~@ c.string c.constant} ...)])
         (λ (x)
           (hash-ref table x (λ ()
                               (raise-arguments-error 'parser
                                                      "string not eligible for parsing"
                                                      "string" x)))))))])
