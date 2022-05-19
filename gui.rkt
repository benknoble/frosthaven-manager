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
         (submod "elements.rkt" gui)
         "monster-db.rkt"
         (submod "monster-db.rkt" gui))

(define (render-manager)
  ;; gui state
  (define/obs @mode 'start)
  ;; game state
  (define/obs @level 0)
  (define/obs @num-players 1)
  ;; list of (cons/c id? (or/c player? monster-group?))
  (define/obs @creatures empty)
  (define/obs @loot-deck empty)
  (define/obs @num-loot-cards 0)
  (define-values (@elements elements-view) (elements-cycler elements))
  (define/obs @in-draw? #f)
  (define/obs @monster-deck (shuffle monster-deck))
  (define/obs @monster-discard empty)
  (define/obs @curses monster-curse-deck)
  (define/obs @modifier #f)
  (define-values (info-db action-db)
    (get-dbs "sample-db.rktd"))
  ;; functions
  (define (make-player-entry i)
    (cons i (make-player "" 1)))
  (define (update-players creatures k f)
    (define (maybe-update-player e)
      (if (~> (e) (-< car cdr) (and% (eq? k) player?))
        (cons k (f (cdr e)))
        e))
    (map maybe-update-player creatures))
  (define (update-monster-groups creatures k f)
    (define (maybe-update-monster-group e)
      (if (~> (e) (-< car cdr) (and% (eq? k) monster-group?))
        (cons k (f (cdr e)))
        e))
    (map maybe-update-monster-group creatures))
  (define (update-all-players creatures f)
    (define update-only-player
      (match-lambda
        [(cons id (? player? p)) (cons id (f p))]
        [c c]))
    (map update-only-player creatures))
  (define (update-all-monster-groups creatures f)
    (define update-only-monster-group
      (match-lambda
        [(cons id (? monster-group? p)) (cons id (f p))]
        [c c]))
    (map update-only-monster-group creatures))
  (define (set-level level)
    (:= @level level))
  (define (set-num-players num-players)
    (:= @num-players num-players))
  (define (to-input-player-info)
    (when (empty? (@! @creatures))
      (:= @creatures (build-list (@! @num-players) make-player-entry)))
    (:= @mode 'input-player-info))
  (define (update-player-name k name)
    (<~@ @creatures (update-players k (player-update-name name))))
  (define (update-player-max-hp k f)
    (<~@ @creatures (update-players k (player-act-on-max-hp f))))
  (define (to-build-loot-deck)
    ;; give each player max-hp
    (<~@ @creatures
         (update-all-players
           (flow (~> (-< (~> player-max-hp const player-act-on-hp)
                         _)
                     apply))))
    (:= @mode 'build-loot-deck))
  (define (to-play)
    (:= @mode 'play)
    ;; HACK: trigger updates in @creatures to re-render list-view (?)
    (:= @creatures (@! @creatures)))
  (define (to-choose-monsters)
    (:= @mode 'choose-monsters))
  (define-flow (update-deck-and-num-loot-cards loot-event)
    (-< (loot-picker-updater @loot-deck)
        ;; order important
        (gen (:= @num-loot-cards (length (@! @loot-deck))))))
  (define (make-player-view k @e)
    (define (update proc)
      (<~@ @creatures (update-players k proc)))
    (define-flow update-player-condition (~> player-condition-handler update))
    (define-flow update-player-hp (~> player-act-on-hp update))
    (define-flow update-player-xp (~> player-act-on-xp update))
    (define (update-player-initiative i) (update (flow (player-set-initiative i))))
    (player-view
      (@> @e cdr)
      @num-players
      #:on-condition update-player-condition
      #:on-hp update-player-hp
      #:on-xp update-player-xp
      #:on-initiative update-player-initiative))
  (define (make-monster-group-view k @e)
    ;; TODO
    (text (@~> @e (~> cdr monster-group-name))))
  (define (take-loot)
    (<@ @loot-deck rest))
  (define (give-player-loot* p)
    (define card
      (@! (@~> @loot-deck (if (not empty?) first #f))))
    (if card
      ((player-add-loot card) p)
      p))
  (define (give-player-loot k)
    (<~@ @creatures (update-players k give-player-loot*)))
  (define (next-round)
    ;; wane elements
    (for-each (flow (<@ wane-element)) @elements)
    ;; reset player initiative
    (<~@ @creatures (update-all-players player-clear-initiative))
    ;; shuffle modifiers if required
    (when (shuffle-modifier-deck? (@! @monster-discard))
      (reshuffle-modifiers))
    ;; toggle state
    (<@ @in-draw? not))
  (define (get-monster-group-initiative mg)
    ;; TODO
    +inf.0)
  (define (draw)
    ;; order players
    (<~@ @creatures
         (sort < #:key (flow
                         (~> cdr
                             (switch
                               [player? player-initiative]
                               [monster-group? get-monster-group-initiative])))))
    ;; toggle state
    (<@ @in-draw? not))
  (define (reshuffle-modifiers)
    (:= @monster-deck (shuffle (append (@! @monster-deck)
                                       (@! @monster-discard))))
    (:= @monster-discard empty))
  (define (draw-modifier)
    ;; better not be empty after this…
    (when (empty? (@! @monster-deck)) (reshuffle-modifiers))
    (define card (first (@! @monster-deck)))
    (:= @modifier card)
    (<@ @monster-deck rest)
    (cond
      [(equal? card curse) (<~@ @curses (cons card _))]
      [else (<~@ @monster-discard (cons card _))]))
  (define (do-curse)
    (unless (empty? (@! @curses))
      (define card (first (@! @curses)))
      (<@ @curses rest)
      (<~@ @monster-deck (cons card _))
      (reshuffle-modifiers)))
  (define add-or-remove-monster-group
    (match-lambda
      [`(add ,mg)
        (define next-id (add1 (apply max (map car (@! @creatures)))))
        (<~@ @creatures (append (list (cons next-id mg))))]
      [`(remove ,mg)
        (<~@ @creatures (remf (flow (~> cdr (equal? mg))) _))]))
  (define (make-creature-view k @e)
    (define make-player-or-monster-group-view
      (match-lambda
        [(cons _ (? player?)) (make-player-view k @e)]
        [(cons _ (? monster-group?)) (make-monster-group-view k @e)]))
    (dyn-view @e make-player-or-monster-group-view))
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
                 (button "Next" to-choose-monsters))]
        [(choose-monsters)
         (vpanel
           (multi-monster-picker
             info-db @level
             #:on-change add-or-remove-monster-group)
           (button "Next" to-play))]
        [(play)
         (vpanel
           ;; top
           (hpanel #:stretch '(#t #f)
                   (spacer)
                   elements-view)
           ;; main
           (group
             "Creatures"
             (list-view @creatures
               #:min-size (@~> @creatures (~>> length (* 100) (list #f)))
               #:key car
               make-creature-view))
           ;; bottom (1)
           (hpanel #:stretch '(#t #f)
                   (button "Next Round" next-round #:enabled? @in-draw?)
                   (spacer)
                   (loot-button
                     @loot-deck @num-loot-cards @num-players
                     (@~> @creatures (filter (flow (~> cdr player?)) _))
                     ;; valid because only enabled if loot-deck non-empty, and only
                     ;; closing if loot assigned
                     #:on-close take-loot
                     #:on-player give-player-loot)
                   (spacer)
                   (button
                     #:enabled? (@~> @curses (not empty?))
                     (@~> @curses
                          (~> length
                              (format "Curse (~a/~a)" _ (length monster-curse-deck))))
                     do-curse)
                   (vpanel
                     (button
                       (@~> @monster-deck (~>> length (format "Draw Modifier (~a)")))
                       draw-modifier)
                     (text (@~> @modifier (~>> (or _ "") (~a "Most Recent Modifier: ")))))
                   (spacer)
                   (button "Draw Action(s)" draw #:enabled? (@> @in-draw? not)))
           ;; bottom (2)
           (hpanel #:stretch '(#f #f)
                   (level-stats @level @num-players)
                   (vpanel
                     (level-table @level)
                     (inspiration-table @num-players))))]
        [else (text "Broken")]))))
