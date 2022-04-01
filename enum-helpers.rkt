#lang racket

(provide
  (contract-out
    [make-property-maker-that-displays-as-constant-names
      (-> uninitialized-enum-descriptor?
          (listof (cons/c struct-type-property? any/c)))]))

(require
  rebellion/type/enum
  rebellion/collection/keyset
  qi)

(define (make-property-maker-that-displays-as-constant-names desc)
  (define default-props-sans-custom-writer
    (~>> (desc)
         default-enum-properties
         (filter (flow (~> car (not (equal? prop:custom-write)))))))
  (define custom-writer (default-enum-custom-write desc))
  (define discrim (enum-descriptor-discriminator desc))
  (define type (enum-descriptor-type desc))
  (define constants (enum-type-constants type))
  (cons (cons prop:custom-write
              (match-lambda**
                ;; display mode
                [(v out #f) (~>> (v)
                                 discrim
                                 (keyset-ref constants)
                                 keyword->string
                                 (display _ out))]
                ;; everything else
                [(v out mode) (custom-writer v out mode)]))
        default-props-sans-custom-writer))
