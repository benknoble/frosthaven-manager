#lang racket

(provide
  (contract-out
    [start-view (->* ()
                     (#:on-level (-> level/c any)
                      #:on-player (-> num-players/c any))
                     (is-a?/c view<%>))]))

(require racket/gui/easy
         frosthaven-manager/defns)

(define (start-view #:on-level [on-level void] #:on-player [on-player void])
  (vpanel
    (spacer)
    (text "Frosthaven Manager")
    (spacer)
    (group
      "Party Information"
      #:stretch '(#f #f)
      (choice #:label "Scenario Level"
              (build-list number-of-levels identity)
              #:choice->label ~a
              on-level)
      (choice #:label "Number of Players"
              (build-list (sub1 max-players) (curry + 2))
              #:choice->label ~a
              on-player))
    (spacer)))

(module+ main
  (require frosthaven-manager/gui/render)
  (void
    ;; no separate eventspace: block main until this window closed
    (render/eventspace
      (window (start-view #:on-level displayln
                          #:on-player displayln)))))
