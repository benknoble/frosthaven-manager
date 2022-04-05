#lang racket

(provide (contract-out
           [start-view (is-a?/c view<%>)]
           [@level (obs/c (integer-in 0 number-of-levels))]
           [@num-players (obs/c (integer-in 1 max-players))]))

(require racket/gui/easy
         racket/gui/easy/operator
         racket/gui/easy/contract
         "defns.rkt")

(define/obs @level 0)
(define/obs @num-players 1)

(define start-view
  (vpanel
    (spacer)
    (text "FROSTHAVEN")
    (spacer)
    (choice #:label "Scenario Level"
            (build-list number-of-levels values)
            #:choice->label ~a
            (λ:= @level identity))
    (choice #:label "Number of Players"
            (build-list max-players add1)
            #:choice->label ~a
            (λ:= @num-players identity))
    (spacer)))

(module+ main
  (void (render (window start-view))))
