#lang racket

(provide
  (contract-out
    [start-view (->* ()
                     (#:on-level (-> natural-number/c any)
                      #:on-player (-> positive-integer? any))
                     (is-a?/c view<%>))]))

(require racket/gui/easy
         "defns.rkt")

(define (start-view #:on-level [on-level void] #:on-player [on-player void])
  (vpanel
    (spacer)
    (text "FROSTHAVEN")
    (spacer)
    (choice #:label "Scenario Level"
            (build-list number-of-levels identity)
            #:choice->label ~a
            on-level)
    (choice #:label "Number of Players"
            (build-list max-players add1)
            #:choice->label ~a
            on-player)
    (spacer)))

(module+ main
  (void (render (window (start-view #:on-level displayln
                                    #:on-player displayln)))))
