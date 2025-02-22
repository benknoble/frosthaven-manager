#lang racket

(provide
 (contract-out
  [level-picker (->* (#:choose (-> level/c any)
                      #:selection (maybe-obs/c level/c))
                     (#:label (maybe-obs/c maybe-label/c))
                    (is-a?/c view<%>))]))

(require frosthaven-manager/defns
         frosthaven-manager/observable-operator
         racket/gui/easy
         racket/gui/easy/contract)

(define (level-picker #:choose on-choose #:selection selection #:label [label #f])
  (choice #:label (@ label)
          (build-list number-of-levels identity)
          #:choice->label ~a
          on-choose
          #:selection (@ selection)))
