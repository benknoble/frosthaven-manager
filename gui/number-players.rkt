#lang racket

(provide
 (contract-out
  [number-players-picker (->* (#:choose (-> level/c any)
                               #:selection (maybe-obs/c level/c))
                              (#:label (maybe-obs/c maybe-label/c))
                              (is-a?/c view<%>))]))

(require racket/gui/easy
         racket/gui/easy/contract

         frosthaven-manager/defns
         frosthaven-manager/observable-operator)

(define (number-players-picker #:choose on-choose #:selection selection #:label [label "Number of Players"])
  (choice #:label (@ label)
          (build-list (sub1 max-players) (curry + 2))
          #:choice->label ~a
          on-choose
          #:selection (@ selection)))
