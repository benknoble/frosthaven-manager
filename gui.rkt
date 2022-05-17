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
  (define-values (@elements elements-view) (elements-cycler elements))
  ;; functions
  (define (make-player-entry i)
    (cons i (make-player "" 1)))
  (define (update-players players k f)
    (define (maybe-update-player e)
      (if (eq? (car e) k)
        (cons k (f (cdr e)))
        e))
    (map maybe-update-player players))
  (define (update-all-players players f)
    (define update-player
      (match-lambda [(cons id p) (cons id (f p))]))
    (map update-player players))
  (define (set-level level)
    (:= @level level))
  (define (set-num-players num-players)
    (:= @num-players num-players))
  (define (to-input-player-info)
    (when (empty? (@! @players))
      (:= @players (build-list (@! @num-players) make-player-entry)))
    (:= @mode 'input-player-info))
  (define (update-player-name k name)
    (<~@ @players (update-players k (player-update-name name))))
  (define (update-player-max-hp k f)
    (<~@ @players (update-players k (player-act-on-max-hp f))))
  (define (to-build-loot-deck)
    ;; give each player max-hp
    (<~@ @players
         (update-all-players
           (flow (~> (-< (~> player-max-hp const player-act-on-hp)
                         _)
                     apply))))
    (:= @mode 'build-loot-deck))
  (define (to-play)
    (:= @mode 'play))
  (define-flow (update-deck-and-num-loot-cards loot-event)
    (-< (loot-picker-updater @loot-deck)
        ;; order important
        (gen (:= @num-loot-cards (length (@! @loot-deck))))))
  (define (make-player-view k @e)
    (define (update proc)
      (<~@ @players (update-players k proc)))
    (define-flow update-condition (~> player-condition-handler update))
    (define-flow update-hp (~> player-act-on-hp update))
    (define-flow update-xp (~> player-act-on-xp update))
    (define (update-initiative i) (update (flow (player-set-initiative i))))
    (player-view
      (@> @e cdr)
      @num-players
      #:on-condition update-condition
      #:on-hp update-hp
      #:on-xp update-xp
      #:on-initiative update-initiative))
  (define (take-loot)
    (<@ @loot-deck rest))
  (define (give-player-loot* p)
    (define card
      (@! (@~> @loot-deck (if (not empty?) first #f))))
    (if card
      ((player-add-loot card) p)
      p))
  (define (give-player-loot k)
    (<~@ @players (update-players k give-player-loot*)))
  (define/obs @in-draw? #f)
  (define (next-round)
    ;; wane elements
    (for-each (flow (<@ wane-element)) @elements)
    ;; reset player initiative
    (<~@ @players (update-all-players player-clear-initiative))
    ;; toggle state
    (<@ @in-draw? not))
  (define (draw)
    ;; order players
    (<~@ @players
         (sort < #:key (flow (~> cdr player-initiative))))
    ;; toggle state
    (<@ @in-draw? not))
  ;; gui
  (render
    (window
      #:title "FROSTHAVEN Manager"
      #:size '(700 300)
      (case-view @mode
        [(start)
         (vpanel (start-view #:on-level set-level
                             #:on-player set-num-players)
                 (button "Play" to-input-player-info))]
        [(input-player-info)
         (vpanel (player-input-views @num-players
                                     #:on-name update-player-name
                                     #:on-hp update-player-max-hp)
                 (button "Next" to-build-loot-deck))]
        [(build-loot-deck)
         (vpanel (loot-picker #:on-card update-deck-and-num-loot-cards)
                 (spacer)
                 (button "Next" to-play))]
        [(play)
         (vpanel
           ;; top
           (hpanel #:stretch '(#t #f)
                   (spacer)
                   elements-view)
           (spacer)
           ;; main
           (group
             "Creatures"
             (list-view @players
               #:min-size (@~> @players (~>> length (* 100) (list #f)))
               #:key car
               make-player-view))
           (spacer)
           ;; bottom (1)
           (hpanel #:stretch '(#t #f)
                   (button "Next Round" next-round #:enabled? @in-draw?)
                   (spacer)
                   (loot-button
                     @loot-deck @num-loot-cards @num-players @players
                     ;; valid because only enabled if loot-deck non-empty, and only
                     ;; closing if loot assigned
                     #:on-close take-loot
                     #:on-player give-player-loot)
                   (spacer)
                   (button "Draw!" draw #:enabled? (@> @in-draw? not)))
           ;; bottom (2)
           (hpanel #:stretch '(#f #f)
                   (level-stats @level @num-players)
                   (vpanel
                     (level-table @level)
                     (inspiration-table @num-players))))]
        [else (text "Broken")]))))
