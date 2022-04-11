#lang racket

(provide render-manager)

(require racket/gui/easy
         racket/gui/easy/operator
         "defns.rkt"
         "start.rkt"
         "player-info.rkt")

(define (render-manager)
  ;; gui state
  (define/obs @mode 'start)
  ;; game state
  (define/obs @level 0)
  (define/obs @num-players 1)
  (define/obs @players empty)
  ;; gui
  (render
    (window
      #:title "FROSTHAVEN Manager"
      (case-view @mode
        [(start)
         (vpanel (start-view #:on-level (λ:= @level identity)
                             #:on-player (λ:= @num-players identity))
                 (button "Play"
                         (thunk (when (empty? (obs-peek @players))
                                  (:= @players (build-list
                                                 (obs-peek @num-players)
                                                 (λ (i) (cons i (@ (make-player "" 1)))))))
                                (:= @mode 'input-player-info))))]
        [(input-player-info) (player-input-views @players)]
        [else (text "Broken")]))))
