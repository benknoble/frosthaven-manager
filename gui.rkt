#lang racket

(provide render-manager)

(require racket/gui/easy
         racket/gui/easy/operator
         #;"defns.rkt"
         "start.rkt"
         "player-info.rkt")

(define (render-manager)
  (define/obs @mode 'start)
  (define-values (@ps p-view) (player-input-views @num-players))
  (render
    (window
      (case-view @mode
        [(start) (vpanel start-view (button "Play" (thunk (:= @mode 'player-info))))]
        [(player-info) p-view]
        [else (text "Broken")]))))
