#lang racket

(provide
 (contract-out
  [round-number-modifier (->* ((obs/c natural-number/c))
                              (#:new-round-number (-> (-> natural-number/c natural-number/c) any))
                              (is-a?/c window-view<%>))]))

(require frosthaven-manager/gui/counter
         frosthaven-manager/gui/mixins
         frosthaven-manager/observable-operator
         racket/gui/easy
         racket/gui/easy/contract)

(define (round-number-modifier @round #:new-round-number [send-new-round void])
  (define-close! close! closing-mixin)
  (dialog
   #:title "Edit round number"
   #:mixin closing-mixin
   (vpanel
    (hpanel
     (button "1" (thunk (send-new-round (const 1))))
     (counter (@> @round {(~a "Round: " _)})
              (thunk (send-new-round add1))
              (thunk (send-new-round {(if (> 1) sub1 1)}))))
    (button "Ok" close!))))
