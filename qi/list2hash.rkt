#lang racket

(provide
  (contract-out
    [list->hash (->* (list?)
                     (#:->key (-> any/c any/c)
                      #:->value (-> any/c any/c))
                     hash?)])
  (for-space qi list~>hash))

(require qi
         syntax/parse/define)

(define (list->hash xs #:->key [->key identity] #:->value [->value identity])
  (for/hash ([x (in-list xs)])
    (on (x) (-< ->key ->value))))

(define-qi-syntax-rule (list~>hash {~optional {~seq #:->key ->key}}
                                   {~optional {~seq #:->value ->value}})
  (list->hash _
              (~? (~@ #:->key (flow ->key)))
              (~? (~@ #:->value (flow ->value)))))
