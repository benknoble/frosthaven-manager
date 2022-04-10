#lang racket

(provide render-manager)

(require racket/gui/easy
         racket/gui/easy/operator
         "defns.rkt"
         "start.rkt"
         "player-info.rkt")

(define (render-manager)
  (define/obs @mode 'start)
  (define/obs @players empty)
  (render
    (window
      (case-view @mode
        [(start)
         (vpanel start-view
                 (button "Play"
                         (thunk (when (empty? (obs-peek @players))
                                  (:= @players (build-list
                                                 (obs-peek @num-players)
                                                 (λ (i) (cons i (@ (make-player "" 1)))))))
                                (:= @mode 'input-player-info))))]
        [(input-player-info) (player-input-views @players)]
        [else (text "Broken")]))))
