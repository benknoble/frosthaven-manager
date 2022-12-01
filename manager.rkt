#lang racket

(module+ main
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
         frosthaven-manager/gui/render
         frosthaven-manager/observable-operator
         frosthaven-manager/qi
         frosthaven-manager/defns
         frosthaven-manager/gui/common-menu
         frosthaven-manager/gui/start
         frosthaven-manager/gui/player-info
         frosthaven-manager/gui/level-info
         frosthaven-manager/gui/loot-picker
         (only-in frosthaven-manager/elements elements)
         frosthaven-manager/gui/elements
         frosthaven-manager/monster-db
         frosthaven-manager/gui/monsters)

;; TODO these functions need a home :(

;; bag of state
(struct state
        [@mode
         @level
         @num-players
         @creatures
         @cards-per-deck
         @loot-deck
         @num-loot-cards
         @elements
         @in-draw?
         @monster-modifier-deck
         @monster-discard
         @curses
         @blesses
         @modifier
         @monster-prev-discard
         @info-db
         @ability-db
         @ability-decks])

(define (make-state @elements
                    [@mode (@ 'start)]
                    [@level (@ 0)]
                    [@num-players (@ 1)]
                    [@creatures (@ empty)]
                    [@cards-per-deck (@ (hash))]
                    [@loot-deck (@ empty)]
                    [@num-loot-cards (@ 0)]
                    [@in-draw? (@ #f)]
                    [@monster-modifier-deck (@ (shuffle monster-modifier-deck))]
                    [@monster-discard (@ empty)]
                    [@curses (@ monster-curse-deck)]
                    [@blesses (@ monster-bless-deck)]
                    [@modifier (@ #f)]
                    [@monster-prev-discard (@ #f)]
                    [@info-db (@ (hash))]
                    [@ability-db (@ (hash))]
                    [@ability-decks (@ (hash))])
  (state @mode
         @level
         @num-players
         @creatures
         @cards-per-deck
         @loot-deck
         @num-loot-cards
         @elements
         @in-draw?
         @monster-modifier-deck
         @monster-discard
         @curses
         @blesses
         @modifier
         @monster-prev-discard
         @info-db
         @ability-db
         @ability-decks))

;; Ability Decks
(struct ability-decks [current draw discard] #:transparent)

(define-flow (ability-decks-draw-next ad)
  (~> (-< (~> ability-decks-draw (and (not empty?) first))
          (~> ability-decks-draw (switch [(not empty?) rest]))
          ability-decks-discard)
      ability-decks))

(define (ability-decks-discard-and-maybe-shuffle ad)
  (match-define (ability-decks current draw discard) ad)
  (define discard-with-current
    (if (monster-ability? current)
      (cons current discard)
      discard))
  (define shuffle?
    (or (empty? draw)
        (on (current)
          (and monster-ability? monster-ability-shuffle?))))
  (define-values (draw* discard*)
    (if shuffle?
      (values (shuffle (append draw discard-with-current)) empty)
      (values draw discard-with-current)))
  (ability-decks #f draw* discard*))

(define ((update-ability-decks f) ads)
  (for/hash ([(set ad) (in-hash ads)])
    (values set (f ad))))

;; Modifier decks
(define (reshuffle-deck @deck @discard)
  (:= @deck (shuffle (append (@! @deck) (@! @discard))))
  (:= @discard empty))

(define (discard @monster-discard @curses @blesses card)
  (cond
    [(equal? card curse) (<~@ @curses (cons card _))]
    [(equal? card bless) (<~@ @blesses (cons card _))]
    [else (<~@ @monster-discard (cons card _))]))

(define ((draw-modifier s))
  ;; better not be empty after this…
  (when (empty? (@! (state-@monster-modifier-deck s)))
    (reshuffle-deck (state-@monster-modifier-deck s) (state-@monster-discard s)))
  (define card (first (@! (state-@monster-modifier-deck s))))
  (:= (state-@monster-prev-discard s) (@! (state-@modifier s)))
  (:= (state-@modifier s) card)
  (<@ (state-@monster-modifier-deck s) rest)
  (discard (state-@monster-discard s) (state-@curses s) (state-@blesses s) card))

(define ((draw-modifier* s [better better-modifier]))
  ;; better not be empty after this…
  (when (~> ((state-@monster-modifier-deck s)) @! length (< 2))
    (reshuffle-deck (state-@monster-modifier-deck s) (state-@monster-discard s)))
  (define cards (~> ((state-@monster-modifier-deck s)) @! (take 2)))
  (define best (~> (cards) sep better))
  (define worst (cond
                  [(equal? best (first cards)) (second cards)]
                  [(equal? best (second cards)) (first cards)]))
  (:= (state-@monster-prev-discard s) worst)
  (:= (state-@modifier s) best)
  (<~@ (state-@monster-modifier-deck s) (drop 2))
  (discard (state-@monster-discard s) (state-@curses s) (state-@blesses s) worst)
  (discard (state-@monster-discard s) (state-@curses s) (state-@blesses s) best))

(define (shuffle-deck @deck)
  (:= @deck (shuffle (@! @deck))))

(define ((deck-adder @cards @deck))
  (unless (empty? (@! @cards))
    (define card (first (@! @cards)))
    (<@ @cards rest)
    (<~@ @deck (cons card _))
    (shuffle-deck @deck)))

;; DBs
(define (init-dbs db s)
  (define-values (info-db ability-db) (get-dbs db))
  (:= (state-@info-db s) info-db)
  (:= (state-@ability-db s) ability-db)
  (:= (state-@ability-decks s)
      (for/hash ([(set abilities) (in-hash ability-db)])
        (values set (ability-decks #f (shuffle abilities) empty)))))

;; Loot
(define ((update-loot-deck-and-num-loot-cards s) evt)
  ((loot-picker-updater (state-@cards-per-deck s)) evt)
  (<@ (state-@num-loot-cards s) (case (car evt) [(add) add1] [(remove) sub1])))

(define ((take-loot s)) (<@ (state-@loot-deck s) rest))

(define ((give-player-loot* s) p)
  (define card
    (@! (@~> (state-@loot-deck s) (and (not empty?) first))))
  (if card
    ((player-add-loot card) p)
    p))

(define ((give-player-loot s) k)
  (<~@ (state-@creatures s) (update-players k (give-player-loot* s))))

;; Creatures
(define (make-player-creature i)
  (creature i (make-player "" 1)))

(define (update-players creatures k f)
  (define (maybe-update-player e)
    (if (~> (e) (-< creature-id creature-v) (and% (eq? k) player?))
      (creature k (f (creature-v e)))
      e))
  (map maybe-update-player creatures))

(define (update-monster-groups creatures k f [fn (flow 1>)])
  (define (maybe-update-monster-group e)
    (if (~> (e)
            (-< creature-id creature-v)
            (and% (eq? k) monster-group*?))
      (let* ([mg* (creature-v e)]
             [n (monster-group*-active mg*)]
             [mg (monster-group*-mg mg*)]
             [new-mg (f mg)]
             [new-n (fn n new-mg)])
        (creature k (monster-group* new-n new-mg)))
      e))
  (map maybe-update-monster-group creatures))

(define (update-all-players creatures f)
  (define (update-only-player c)
    (match c
      [(creature id (? player? p)) (creature id (f p))]
      [c c]))
  (map update-only-player creatures))

(define (update-all-monster-groups creatures f)
  (define (update-only-monster-group c)
    (match c
      [(creature id (monster-group* n mg))
       (creature id (monster-group* n (f mg)))]
      [c c]))
  (map update-only-monster-group creatures))

(define ((update-player-name s) k name)
  (<~@ (state-@creatures s) (update-players k (player-update-name name))))

(define ((update-player-max-hp s) k f)
  (<~@ (state-@creatures s) (update-players k (player-act-on-max-hp f))))

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

(define ((monster-group-initiative s) mg)
  (~> ((state-@ability-decks s) mg)
      (== @! monster-group-set-name)
      hash-ref
      ability-decks-current
      (switch
        [monster-ability? monster-ability-initiative]
        [else +inf.0])))

(define (monster-group*-initiative s)
  (flow (~> monster-group*-mg (monster-group-initiative s))))

(define (creature-initiative s)
  (flow (~> creature-v
            (switch
              [player? player-initiative]
              [monster-group*? (esc (monster-group*-initiative s))]))))

;; (-> mg (-> creature bool))
(define-flow creature-is-mg~?
  (clos (~>
          (== _ (~> creature-v (and monster-group*? monster-group*-mg)))
          equal?)))

(define ((add-or-remove-monster-group s) evt)
  (match evt
    [`(add ,mg)
      (define next-id (~> ((state-@creatures s)) @! sep (>< creature-id) max add1))
      (define selection
        (~> (mg) monster-group-monsters
            (and (not empty?) (~> first monster-number))))
      (<~@ (state-@creatures s)
           (append
             (list (creature next-id (monster-group* selection mg)))))]
    [`(remove ,mg) (<~@ (state-@creatures s) (remf (creature-is-mg~? mg) _))]))

(define ((make-creature-view s) k @e)
  (cond-view
    [(@~> @e (~> creature-v player?)) ((make-player-view s) k @e)]
    [(@~> @e (~> creature-v monster-group*?)) ((make-monster-group-view s) k @e)]
    [else (text "creature is neither player or monster-group*")]))

;; Transition functions
(define ((to-input-player-info s))
  (when (empty? (@! (state-@creatures s)))
    (:= (state-@creatures s) (build-list (@! (state-@num-players s)) make-player-creature)))
  (:= (state-@mode s) 'input-player-info))

(define ((to-build-loot-deck s))
  ;; give each player max-hp
  (<~@ (state-@creatures s)
        (update-all-players
          (flow (~> (-< (~> player-max-hp const player-act-on-hp)
                        _)
                    apply))))
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
    (reshuffle-deck (state-@monster-modifier-deck s) (state-@monster-discard s)))
  ;; toggle state
  (<@ (state-@in-draw? s) not))

(define ((draw-abilities s))
  ;; draw new monster cards
  (<@ (state-@ability-decks s) (update-ability-decks ability-decks-draw-next))
  ;; order creatures
  (<~@ (state-@creatures s) (sort < #:key (creature-initiative s)))
  ;; toggle state
  (<@ (state-@in-draw? s) not))

;; GUI
(define (deck-adder-button @cards do-adder text original-deck)
  (button
    #:enabled? (@~> @cards (not empty?))
    (@~> @cards
         (~> length
             (format "~a (~a/~a)" text _ (length original-deck))))
    do-adder))

(define (manager)
  (define-values (@elements elements-view) (elements-cycler elements vpanel))
  (define s (make-state @elements))
  ;; functions
  ;; gui
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
      (button "Open Monster DB"
              (thunk
                (init-dbs (or (get-file "Monster DB") default-monster-db) s)))
      (button "Use Default Monster DB" (thunk (init-dbs default-monster-db s)))
      (spacer))
    (button "Next" (to-choose-monsters s) #:enabled? (@~> (state-@info-db s) (not hash-empty?)))))

(define (choose-monsters-view s)
  (vpanel
    (multi-monster-picker (state-@info-db s) (state-@level s) #:on-change (add-or-remove-monster-group s))
    (button "Next" (to-play s))))

(define (play-view s elements-view)
  (define do-curse-monster (deck-adder (state-@curses s) (state-@monster-modifier-deck s)))
  (define do-bless-monster (deck-adder (state-@blesses s) (state-@monster-modifier-deck s)))
  (vpanel
    (hpanel
      ;; left
      elements-view
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
        (deck-adder-button
          (state-@curses s) do-curse-monster "Curse Monster" monster-curse-deck)
        (deck-adder-button
          (state-@blesses s) do-bless-monster "Bless Monster" monster-bless-deck)
        (spacer)
        (button
          (@~> (state-@monster-modifier-deck s)
               (~>> length (format "Draw Modifier (~a)")))
          (draw-modifier s))
        (button "Advantage" (draw-modifier* s))
        (button "Disadvantage" (draw-modifier* s worse-modifier))
        (text (@~> (state-@modifier s)
                   (~>> (or _ "") (~a "Most Recent Modifier: "))))
        (text (@~> (state-@monster-prev-discard s)
                   (~>> (or _ "") (~a "Previous Modifier: "))))
        (spacer)
        (button "Next Round" #:enabled? (state-@in-draw? s) (next-round s))
        (button "Draw Abilities" #:enabled? (@> (state-@in-draw? s) not) (draw-abilities s))))
    ;; bottom
    (hpanel #:stretch '(#f #f)
            (loot-button
              (state-@loot-deck s) (state-@num-loot-cards s) (state-@num-players s)
              (@~> (state-@creatures s) (filter (flow (~> creature-v player?)) _))
              ;; valid because only enabled if loot-deck non-empty, and
              ;; only closing if loot assigned
              #:on-close (take-loot s)
              #:on-player (give-player-loot s))
            (level-stats (state-@level s) (state-@num-players s))
            (level-table (state-@level s))
            (inspiration-table (state-@num-players s)))))
