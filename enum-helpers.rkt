#lang racket

(provide
  define-serializable-enum-type
  (contract-out
    [make-property-maker-that-displays-as-constant-names
      (-> uninitialized-enum-descriptor?
          (listof (cons/c struct-type-property? any/c)))]))

(require
  racket/serialize
  (for-syntax racket/syntax)
  syntax/parse/define
  rebellion/type/enum
  rebellion/collection/keyset
  frosthaven-manager/qi)

(define (make-property-maker-that-displays-as-constant-names desc)
  (define default-props-sans-custom-writer
    (remove prop:custom-write
            (default-enum-properties desc)
            (match-lambda** [(key (cons prop _)) (equal? key prop)])))
  (define custom-writer (default-enum-custom-write desc))
  (define discrim (enum-descriptor-discriminator desc))
  (define type (enum-descriptor-type desc))
  (define constants (enum-type-constants type))
  (cons (cons prop:custom-write
              (match-lambda**
                ;; display mode
                [(v out #f) (display (~>> (v)
                                          discrim
                                          (keyset-ref constants)
                                          keyword->string)
                                     out)]
                ;; everything else
                [(v out mode) (custom-writer v out mode)]))
        default-props-sans-custom-writer))

(define-syntax-parse-rule
  (define-serializable-enum-type
    type:id
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
        (thunk (error 'type "cycles not supported"))))))

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
