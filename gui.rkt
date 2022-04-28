#lang racket

(provide render-manager)

(require racket/gui/easy
         "observable-operator.rkt"
         "qi.rkt"
         "defns.rkt"
         "start.rkt"
         "player-info.rkt"
         "loot.rkt"
         (only-in "elements.rkt" elements)
         (submod "elements.rkt" gui))

(define (render-manager)
  ;; gui state
  (define/obs @mode 'start)
  ;; game state
  (define/obs @level 0)
  (define/obs @num-players 1)
  (define/obs @players empty)
  (define/obs @loot-deck empty)
  (define/obs @num-loot-cards #f)
  ;; gui
  (render
    (window
      #:title "FROSTHAVEN Manager"
      #:size '(700 300)
      (case-view @mode
        [(start)
         (vpanel (start-view #:on-level (λ:= @level identity)
                             #:on-player (λ:= @num-players identity))
                 (button "Play"
                         (thunk (when (empty? (@! @players))
                                  (:= @players (build-list
                                                 (@! @num-players)
                                                 (λ (i) (cons i (@ (make-player "" 1)))))))
                                (:= @mode 'input-player-info))))]
        [(input-player-info)
         (vpanel (player-input-views @players)
                 (button "Next"
                         (thunk (:= @mode 'build-loot-deck))))]
        [(build-loot-deck)
         (vpanel (loot-picker #:on-card (loot-picker-updater @loot-deck))
                 (spacer)
                 (button "Next"
                         (thunk (println (list @level @num-players @players @loot-deck))
                                (:= @num-loot-cards (length (@! @loot-deck)))
                                (:= @mode 'play))))]
        [(play)
         (let-values ([(@elements elements-view) (elements-cycler elements)])
           (vpanel
             (hpanel
               #:stretch '(#t #f)
               (spacer)
               elements-view)
             (spacer)
             (hpanel
               #:stretch '(#t #f)
               (button "Next Round"
                       (thunk
                         ;; wane elements
                         (for-each (flow (<@ wane-element)) @elements)
                         ))
               (spacer)
               (button (obs-combine (flow (~>> (== length (or _ 0)) (format "Loot (~a/~a)!")))
                                    @loot-deck @num-loot-cards)
                       #:enabled? (@~> @loot-deck (not empty?))
                       (thunk
                         (render
                           (dialog #:title "Loot card"
                                   #:size '(250 100)
                                   (text (@~> @loot-deck
                                              (if (not empty?)
                                                (~> first (format-loot-card (@! @num-players)))
                                                "")))))
                         (<@ @loot-deck rest)))
               (spacer))
             ))]
        [else (text "Broken")]))))
