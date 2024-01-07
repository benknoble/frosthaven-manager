#lang racket

(provide
  (contract-out
    [start-view (->* ((obs/c level/c) (obs/c num-players/c))
                     (#:on-level (-> level/c any)
                      #:on-player (-> num-players/c any))
                     (is-a?/c view<%>))]))

(require (only-in racket/gui normal-control-font)
         racket/gui/easy
         racket/gui/easy/contract
         frosthaven-manager/defns
         frosthaven-manager/gui/font)

(define (start-view @level @num-players #:on-level [on-level void] #:on-player [on-player void])
  (vpanel
    (spacer)
    (text "Frosthaven Manager"
          #:font (copy-font normal-control-font #:size 50))
    (spacer)
    (group
      "Party Information"
      #:stretch '(#f #f)
      (choice #:label "Scenario Level"
              (build-list number-of-levels identity)
              #:choice->label ~a
              on-level
              #:selection @level)
      (choice #:label "Number of Players"
              (build-list (sub1 max-players) (curry + 2))
              #:choice->label ~a
              on-player
              #:selection @num-players))
    (spacer)))

(module+ main
  (require frosthaven-manager/gui/render)
  (void
    ;; no separate eventspace: block main until this window closed
    (render/eventspace
      (window (start-view #:on-level displayln
                          #:on-player displayln)))))
