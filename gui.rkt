#lang racket

(provide render-manager)

(require racket/gui/easy
         "observable-operator.rkt"
         "defns.rkt"
         "start.rkt"
         "player-info.rkt"
         "loot.rkt")

(define (render-manager)
  ;; gui state
  (define/obs @mode 'start)
  ;; game state
  (define/obs @level 0)
  (define/obs @num-players 1)
  (define/obs @players empty)
  (define/obs @loot-deck empty)
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
                 (button "Next" (thunk (displayln (list @level @num-players @players @loot-deck)))))]
        [else (text "Broken")]))))
