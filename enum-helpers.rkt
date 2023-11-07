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

(module+ test (require rackunit))

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

(module+ test
  (define-enum-type langs (racket typed/racket datalog) #:property-maker make-property-maker-that-displays-as-constant-names)
  (define-binary-check (check-display-output actual expected)
                       (equal? (with-output-to-string (thunk (display actual)))
                               expected))
  (test-case "displayble enums"
    (check-display-output racket "racket")
    (check-display-output typed/racket "typed/racket")
    (check-display-output datalog "datalog")
    ;;
    (check-equal? (~a racket) "racket")
    (check-equal? (~a typed/racket) "typed/racket")
    (check-equal? (~a datalog) "datalog")
    ;;
    (check-equal? (~s racket) "#<langs:racket>")
    (check-equal? (~s typed/racket) "#<langs:typed/racket>")
    (check-equal? (~s datalog) "#<langs:datalog>")))

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
