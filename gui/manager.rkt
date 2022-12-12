#lang racket

(module+ main
  (require frosthaven-manager/gui/render)
  ;; (require racket/gui/easy/debugger)
  ;; (start-debugger)
  (void (render/eventspace
          ;; no separate eventspace: block main until this window closed
          (manager))))

(provide manager)

(require racket/gui/easy
         (only-in racket/gui
                  get-file
                  application-about-handler)
         frosthaven-manager/observable-operator
         frosthaven-manager/qi
         frosthaven-manager/defns
         frosthaven-manager/manager
         frosthaven-manager/gui/common-menu
         frosthaven-manager/gui/start
         frosthaven-manager/gui/player-info
         frosthaven-manager/gui/level-info
         frosthaven-manager/gui/loot-picker
         (only-in frosthaven-manager/elements elements)
         frosthaven-manager/gui/elements
         frosthaven-manager/monster-db
         frosthaven-manager/gui/monsters)

(define (manager)
  (define-values (@elements elements-view) (elements-cycler elements vpanel))
  (define s (make-state @elements))
  (application-about-handler do-about)
  (window
    #:title "Frosthaven Manager"
    #:size '(800 600)
    (menu-bar
      (menu "Help"
            (about-menu-item)
            (how-to-play-menu-item)
            (menu-item-separator)
            (send-feedback-menu-item)
            (issue-menu-item)
            (feature-menu-item)
            (contribute-menu-item)))
    (case-view (state-@mode s)
      [(start) (the-start-view s)]
      [(input-player-info) (input-player-info-view s)]
      [(build-loot-deck) (build-loot-deck-view s)]
      [(choose-monster-db) (choose-monster-db-view s)]
      [(choose-monsters) (choose-monsters-view s)]
      [(play) (play-view s elements-view)]
      [else (text "Broken")])))

;;;; GUI

(define (the-start-view s)
  (vpanel
    (start-view #:on-level (λ:= (state-@level s))
                #:on-player (λ:= (state-@num-players s)))
    (button "Play" (to-input-player-info s))))

(define (input-player-info-view s)
  (vpanel
    (player-input-views (state-@num-players s)
                        #:on-name (update-player-name s)
                        #:on-hp (update-player-max-hp s))
    (button "Next" (to-build-loot-deck s))))

(define (build-loot-deck-view s)
  (vpanel
    (loot-picker #:on-card (update-loot-deck-and-num-loot-cards s))
    (spacer)
    (button "Next" (to-choose-monster-db s))))

(define (choose-monster-db-view s)
  (vpanel
    (db-view (state-@info-db s) (state-@ability-db s))
    (hpanel
      #:stretch '(#t #f)
      (spacer)
      (button "Open Bestiary"
              (thunk
                (init-dbs (or (get-file "Bestiary") default-monster-db) s)))
      (button "Use Default Bestiary" (thunk (init-dbs default-monster-db s)))
      (spacer))
    (button "Next" (to-choose-monsters s) #:enabled? (@~> (state-@info-db s) (not hash-empty?)))))

(define (choose-monsters-view s)
  (vpanel
    (multi-monster-picker (state-@info-db s) (state-@level s) #:on-change (add-or-remove-monster-group s))
    (button "Next" (to-play s))))

(define (play-view s elements-view)
  (vpanel
    (hpanel
      ;; left
      (vpanel
        elements-view
        (button "Infuse All" (thunk (infuse-all (state-@elements s))))
        (button "Consume All" (thunk (consume-all (state-@elements s)))))
      ;; main
      (group
        "Creatures"
        (list-view (state-@creatures s)
          #:min-size (@~> (state-@creatures s) (~>> length (* 100) (list #f)))
          #:key creature-id
          (make-creature-view s)))
      ;; right
      (vpanel
        #:stretch '(#f #t)
        (deck-adder-button (state-@curses s) (do-curse-monster s) "Curse Monster" monster-curse-deck)
        (deck-adder-button (state-@blesses s) (do-bless-monster s) "Bless Monster" monster-bless-deck)
        (spacer)
        (button (@~> (state-@monster-modifier-deck s) (~>> length (format "Draw Modifier (~a)")))
                (draw-modifier s))
        (button "Advantage" (draw-modifier* s))
        (button "Disadvantage" (draw-modifier* s worse-modifier))
        (text (@~> (state-@modifier s) (~>> (or _ "") (~a "Most Recent Modifier: "))))
        (text (@~> (state-@monster-prev-discard s) (~>> (or _ "") (~a "Previous Modifier: "))))
        (button "Show Discard Pile" (show-discard-pile s))
        (spacer)
        (text (@~> (state-@round s) (~a "Round: " _)))
        (button "Next Round" #:enabled? (state-@in-draw? s) (next-round s))
        (button "Draw Abilities" #:enabled? (@> (state-@in-draw? s) not) (draw-abilities s))))
    ;; bottom
    (hpanel #:stretch '(#f #f)
            (loot-button (state-@loot-deck s)
                         (state-@num-loot-cards s)
                         (state-@num-players s)
                         (@~> (state-@creatures s)
                              (~> sep (pass (~> creature-v player?))
                                  (>< (~> (-< creature-v creature-id) cons)) collect))
                         #:on-close (take-loot s)
                         #:on-player (give-player-loot s))
            (level-stats (state-@level s) (state-@num-players s))
            (level-table (state-@level s))
            (inspiration-table (state-@num-players s)))))

(define (deck-adder-button @cards do-adder text original-deck)
  (button
    #:enabled? (@~> @cards (not empty?))
    (@~> @cards
         (~> length
             (format "~a (~a/~a)" text _ (length original-deck))))
    do-adder))

(define ((make-player-view s) k @e)
  (define (update proc)
    (<~@ (state-@creatures s) (update-players k proc)))
  (define-flow update-player-condition (~> player-condition-handler update))
  (define-flow update-player-hp (~> player-act-on-hp update))
  (define-flow update-player-xp (~> player-act-on-xp update))
  (define (update-player-initiative i)
    (update (flow (player-set-initiative i))))
  (player-view
    (@> @e creature-v)
    (state-@num-players s)
    #:on-condition update-player-condition
    #:on-hp update-player-hp
    #:on-xp update-player-xp
    #:on-initiative update-player-initiative))

(define ((make-monster-group-view s) k @e)
  (define (update proc [procn (flow 1>)])
    (<~@ (state-@creatures s) (update-monster-groups k proc procn)))
  (define (update-by-num num proc)
    (update (monster-group-update-num num proc)))
  (define @mg* (@> @e creature-v))
  (define @mg (@> @mg* monster-group*-mg))
  (define @n (@> @mg* monster-group*-active))
  (define @ms (@> @mg monster-group-monsters))
  (define (update-condition num c on?)
    (update-by-num num (monster-update-condition c on?)))
  (define (update-hp num proc)
    (update-by-num num (monster-update-hp proc)))
  (define (kill num)
    (update (monster-group-remove num)
            (flow (~> 2> monster-group-first-monster))))
  (define (new num elite?)
    (update (monster-group-add num elite?)
            (const num)))
  (define (select num) (update values (const num)))
  (define @ability
    (obs-combine
      (flow (~> (== _ monster-group-set-name) hash-ref ability-decks-current))
      (state-@ability-decks s) @mg))
  (monster-group-view
    @mg
    @ability
    @n
    #:on-condition update-condition
    #:on-hp update-hp
    #:on-kill kill
    #:on-new new
    #:on-select select))

(define ((make-creature-view s) k @e)
  (cond-view
    [(@~> @e (~> creature-v player?)) ((make-player-view s) k @e)]
    [(@~> @e (~> creature-v monster-group*?)) ((make-monster-group-view s) k @e)]
    [else (text "creature is neither player or monster-group*")]))

(define ((show-discard-pile s))
  (render
    (dialog
      #:title "Discard Pile"
      #:min-size (@~> (state-@monster-discard s) (~>> length (* 30) (list 200)))
      (text "(Most recent first)")
      (spacer)
      (text (@~> (state-@monster-discard s) (~>> (map ~a) (string-join _ "\n"))))
      (spacer))))

;;;; Transition functions

(define ((to-input-player-info s))
  (when (empty? (@! (state-@creatures s)))
    (:= (state-@creatures s) (build-list (@! (state-@num-players s)) make-player-creature)))
  (:= (state-@mode s) 'input-player-info))

(define ((to-build-loot-deck s))
  ;; give each player max-hp
  (<~@ (state-@creatures s)
        (update-all-players
          (flow (~> (-< (~> player-max-hp const player-act-on-hp) _) apply))))
  (:= (state-@mode s) 'build-loot-deck))

(define ((to-play s))
  (:= (state-@mode s) 'play)
  ;; HACK: trigger updates in (state-@creatures s) to re-render list-view (?)
  (:= (state-@creatures s) (@! (state-@creatures s))))

(define ((to-choose-monster-db s))
  (:= (state-@loot-deck s) (build-loot-deck (@! (state-@cards-per-deck s))))
  (:= (state-@mode s) 'choose-monster-db))
(define ((to-choose-monsters s))
  (:= (state-@mode s) 'choose-monsters))

(define ((next-round s))
  ;; wane elements
  (for-each (flow (<@ wane-element)) (state-@elements s))
  ;; reset player initiative
  (<~@ (state-@creatures s) (update-all-players player-clear-initiative))
  ;; discard monster cards
  (<@ (state-@ability-decks s)
      (update-ability-decks ability-decks-discard-and-maybe-shuffle))
  ;; order creatures
  (<~@ (state-@creatures s) (sort < #:key (creature-initiative s)))
  ;; shuffle modifiers if required
  (when (shuffle-modifier-deck? (@! (state-@monster-discard s)))
    (reshuffle-modifier-deck s))
  ;; increment round number
  (<@ (state-@round s) add1)
  ;; toggle state
  (<@ (state-@in-draw? s) not))

(define ((draw-abilities s))
  ;; draw new monster cards
  (<@ (state-@ability-decks s) (update-ability-decks ability-decks-draw-next))
  ;; order creatures
  (<~@ (state-@creatures s) (sort < #:key (creature-initiative s)))
  ;; toggle state
  (<@ (state-@in-draw? s) not))
