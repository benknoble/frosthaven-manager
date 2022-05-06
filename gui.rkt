#lang racket

(provide render-manager)

(require racket/gui/easy
         "observable-operator.rkt"
         "qi.rkt"
         "defns.rkt"
         "start.rkt"
         "player-info.rkt"
         "level-info.rkt"
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
  (define/obs @num-loot-cards 0)
  (define (update-players players k f)
    (map (λ (e)
           (if (eq? (car e) k)
             (cons k (f (cdr e)))
             e))
         players))
  (define (update-all-players players f)
    (map (match-lambda [(cons id p) (cons id (f p))]) players))
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
                                                 (λ (i) (cons i (make-player "" 1))))))
                                (:= @mode 'input-player-info))))]
        [(input-player-info)
         (vpanel
           (player-input-views
             @num-players
             #:on-name (λ (k name)
                         (<~@ @players (update-players k (update-name name))))
             #:on-hp (λ (k f)
                       (<~@ @players (update-players k (act-on-max-hp f)))))
           (button "Next"
                   (thunk
                     ;; give each player max-hp
                     (<~@ @players
                          (update-all-players
                            (flow (~> (-< (~> player-max-hp const act-on-hp)
                                          _)
                                      apply))))
                     (:= @mode 'build-loot-deck))))]
        [(build-loot-deck)
         (vpanel (loot-picker #:on-card (flow (-< (loot-picker-updater @loot-deck)
                                                  ;; order important
                                                  (gen (:= @num-loot-cards (length (@! @loot-deck)))))))
                 (spacer)
                 (button "Next"
                         (thunk (:= @mode 'play))))]
        [(play)
         (let-values ([(@elements elements-view) (elements-cycler elements)])
           (vpanel
             (hpanel
               #:stretch '(#t #f)
               (spacer)
               elements-view)
             (spacer)
             (list-view @players
               #:min-size (@~> @players (~>> length (* 100) (list #f)))
               #:key car
               (λ (k @e)
                 (define (update proc)
                   (<~@ @players (update-players k proc)))
                 (player-view
                   (@> @e cdr)
                   @num-players
                   #:on-condition (flow (~> condition-handler update))
                   #:on-hp (flow (~> act-on-hp update))
                   #:on-xp (flow (~> act-on-xp update))
                   #:on-initiative (λ (i) (update (flow (set-initiative i)))))))
             (spacer)
             (hpanel
               #:stretch '(#t #f)
               (button "Next Round"
                       (thunk
                         ;; wane elements
                         (for-each (flow (<@ wane-element)) @elements)
                         ;; reset player initiative
                         (<~@ @players (update-all-players clear-initiative))))
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
               (spacer)
               (button "Draw!"
                       (thunk
                         ;; order players
                         (<~@ @players
                              (sort < #:key (flow (~> cdr player-initiative)))))))
             (hpanel
               #:stretch '(#f #f)
               (level-stats @level @num-players)
               (vpanel
                 (level-table @level)
                 (inspiration-table @num-players)))))]
        [else (text "Broken")]))))
