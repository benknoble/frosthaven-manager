#lang racket

(provide
  define-serializable-enum-type)

(require
  racket/serialize
  (for-syntax racket/syntax)
  syntax/parse/define
  rebellion/type/enum
  rebellion/collection/keyset
  frosthaven-manager/curlique)

(module+ test (require rackunit))

(define-syntax-parser define-serializable-enum-type
  [(_ type:id
      (constant:id ...)
      {~alt
       {~optional {~and #:omit-root-binding omit-root-binding-kw}}
       {~optional {~seq #:descriptor-name descriptor:id}}
       {~optional {~seq #:predicate-name predicate:id}}
       {~optional {~seq #:discriminator-name discriminator:id}}
       {~optional {~seq #:selector-name selector:id}}
       {~optional {~seq #:inspector inspector:expr}}
       {~optional {~seq #:property-maker prop-maker:expr}}} ...)
   #:with deserialize-info (format-id #'type "deserialize-info:~a" #'type #:source #'type)
   #:with default-selector (format-id #'type "selector:~a" #'type)
   (syntax/loc this-syntax
     (begin
       (define-enum-type type
         (constant ...)
         {~? omit-root-binding-kw}
         {~? {~@ #:descriptor-name descriptor}}
         {~? {~@ #:predicate-name predicate}}
         {~? {~@ #:discriminator-name discriminator}}
         {~? {~@ #:selector-name selector}}
         {~? {~@ #:inspector inspector}}
         #:property-maker (compose-property-makers
                           {~? prop-maker default-enum-properties}
                           (serializable-property-maker #'deserialize-info)))
       (provide deserialize-info)
       (define deserialize-info
         (make-deserialize-info
          {~? selector default-selector}
          (thunk (error 'type "cycles not supported"))))))])

(module+ test
  (require racket/serialize)
  (define-serializable-enum-type slangs (j apl forth))
  (define-simple-check (check-serializes x)
                       (equal? x (deserialize (serialize x))))
  (test-case "serializable enums"
    (check-serializes j "j")
    (check-serializes apl "apl")
    (check-serializes forth "forth")))

(define ((serializable-property-maker deserialize-info-binding) desc)
  (define discrim (enum-descriptor-discriminator desc))
  (list (cons prop:serializable
              (make-serialize-info
                (λ (x) (vector (discrim x)))
                deserialize-info-binding
                #f
                (or (current-load-relative-directory) (current-directory))))))

(define ((compose-property-makers . ps) desc)
  (append-map (λ (p) (p desc)) ps))
