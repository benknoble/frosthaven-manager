#lang racket

(provide render-manager)

(require racket/gui/easy
         (only-in racket/gui get-file)
         "observable-operator.rkt"
         "qi.rkt"
         "defns.rkt"
         "menu.rkt"
         "start.rkt"
         "player-info.rkt"
         "level-info.rkt"
         "loot.rkt"
         (only-in "elements.rkt" elements)
         (submod "elements.rkt" gui)
         "monster-db.rkt"
         (submod "monster-db.rkt" gui))

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
    (if (monster-action? current)
      (cons current discard)
      discard))
  (define shuffle?
    (or (empty? draw)
        (on (current)
          (and monster-action? monster-action-shuffle?))))
  (define-values (draw* discard*)
    (if shuffle?
      (values (shuffle (append draw discard-with-current)) empty)
      (values draw discard-with-current)))
  (ability-decks #f draw* discard*))

(define ((update-ability-decks f) ads)
  (for/hash ([(set ad) (in-hash ads)])
    (values set (f ad))))

;; DBs
(define (init-dbs db @info-db @action-db @ability-decks)
  (define-values (info-db action-db) (get-dbs db))
  (:= @info-db info-db)
  (:= @action-db action-db)
  (:= @ability-decks
      (for/hash ([(set actions) (in-hash action-db)])
        (values set (ability-decks #f (shuffle actions) empty)))))

;; Loot
(define (update-deck-and-num-loot-cards @loot-deck @num-loot-cards)
  (flow (-< (loot-picker-updater @loot-deck)
            ;; order important
            (gen (:= @num-loot-cards (length (@! @loot-deck)))))))

(define ((take-loot @loot-deck)) (<@ @loot-deck rest))

(define ((give-player-loot* @loot-deck) p)
  (define card
    (@! (@~> @loot-deck (and (not empty?) first))))
  (if card
    ((player-add-loot card) p)
    p))

(define ((give-player-loot @creatures @loot-deck) k)
  (<~@ @creatures (update-players k (give-player-loot* @loot-deck))))

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
      [(creature id (monster-group* n mg)) (creature id (monster-group* n (f mg)))]
      [c c]))
  (map update-only-monster-group creatures))

(define ((update-player-name @creatures) k name)
  (<~@ @creatures (update-players k (player-update-name name))))

(define ((update-player-max-hp @creatures) k f)
  (<~@ @creatures (update-players k (player-act-on-max-hp f))))

(define ((make-player-view @creatures @num-players) k @e)
  (define (update proc)
    (<~@ @creatures (update-players k proc)))
  (define-flow update-player-condition (~> player-condition-handler update))
  (define-flow update-player-hp (~> player-act-on-hp update))
  (define-flow update-player-xp (~> player-act-on-xp update))
  (define (update-player-initiative i) (update (flow (player-set-initiative i))))
  (player-view
    (@> @e creature-v)
    @num-players
    #:on-condition update-player-condition
    #:on-hp update-player-hp
    #:on-xp update-player-xp
    #:on-initiative update-player-initiative))

(define ((make-monster-group-view @creatures) k @e ads)
  (define (update proc [procn (flow 1>)])
    (<~@ @creatures (update-monster-groups k proc procn)))
  (define @mg* (@> @e creature-v))
  (define @mg (@> @mg* monster-group*-mg))
  (define @n (@> @mg* monster-group*-active))
  (define @ms (@> @mg monster-group-monsters))
  (define (update-condition num c on?)
    (update (monster-group-update-num num (monster-update-condition c on?))))
  (define (update-hp num proc)
    (update (monster-group-update-num num (monster-update-hp proc))))
  (define (kill num)
    (update (monster-group-remove num) (flow (~> 2> monster-group-first-monster))))
  (define (new num elite?) (update (monster-group-add num elite?) (const num)))
  (define (select num) (update values (const num)))
  (define @action (@~> @mg (~>> monster-group-set-name
                                (hash-ref ads)
                                ability-decks-current)))
  (monster-group-view
    @mg
    @action
    @n
    #:on-condition update-condition
    #:on-hp update-hp
    #:on-kill kill
    #:on-new new
    #:on-select select))

(define ((monster-group-initiative @ability-decks) mg)
  (~> (@ability-decks mg)
      (== @! monster-group-set-name)
      hash-ref
      ability-decks-current
      (switch
        [monster-action? monster-action-initiative]
        [else +inf.0])))

(define (monster-group*-initiative @ability-decks)
  (flow (~> monster-group*-mg (monster-group-initiative @ability-decks))))

(define (creature-initiative @ability-decks)
  (flow (~> creature-v
            (switch
              [player? player-initiative]
              [monster-group*? (monster-group*-initiative @ability-decks)]))))

;; Transition functions
(define ((to-input-player-info @mode @creatures @num-players))
  (when (empty? (@! @creatures))
    (:= @creatures (build-list (@! @num-players) make-player-creature)))
  (:= @mode 'input-player-info))

(define ((to-build-loot-deck @mode @creatures))
  ;; give each player max-hp
  (<~@ @creatures
        (update-all-players
          (flow (~> (-< (~> player-max-hp const player-act-on-hp)
                        _)
                    apply))))
  (:= @mode 'build-loot-deck))

(define ((to-play @mode @creatures))
  (:= @mode 'play)
  ;; HACK: trigger updates in @creatures to re-render list-view (?)
  (:= @creatures (@! @creatures)))

(define ((to-choose-monster-db @mode))
  (:= @mode 'choose-monster-db))
(define ((to-choose-monsters @mode))
  (:= @mode 'choose-monsters))

(define (render-manager)
  ;; gui state
  (define/obs @mode 'start)
  ;; game state
  (define/obs @level 0)
  (define/obs @num-players 1)
  (define/obs @creatures empty)
  (define/obs @loot-deck empty)
  (define/obs @num-loot-cards 0)
  (define-values (@elements elements-view) (elements-cycler elements vpanel))
  (define/obs @in-draw? #f)
  (define/obs @monster-modifier-deck (shuffle monster-modifier-deck))
  (define/obs @monster-discard empty)
  (define/obs @curses monster-curse-deck)
  (define/obs @blesses monster-bless-deck)
  (define/obs @modifier #f)
  (define/obs @monster-prev-discard #f)
  (define/obs @info-db (hash))
  (define/obs @action-db (hash))
  (define/obs @ability-decks (hash))
  ;; functions
  (define (next-round)
    ;; wane elements
    (for-each (flow (<@ wane-element)) @elements)
    ;; reset player initiative
    (<~@ @creatures (update-all-players player-clear-initiative))
    ;; discard monster cards
    (<@ @ability-decks (update-ability-decks ability-decks-discard-and-maybe-shuffle))
    ;; order creatures
    (<~@ @creatures (sort < #:key (creature-initiative @ability-decks)))
    ;; shuffle modifiers if required
    (when (shuffle-modifier-deck? (@! @monster-discard))
      (reshuffle-modifiers))
    ;; toggle state
    (<@ @in-draw? not))
  (define (draw)
    ;; draw new monster cards
    (<@ @ability-decks (update-ability-decks ability-decks-draw-next))
    ;; order creatures
    (<~@ @creatures (sort < #:key creature-initiative))
    ;; toggle state
    (<@ @in-draw? not))
  (define (reshuffle-modifiers)
    (:= @monster-modifier-deck (shuffle (append (@! @monster-modifier-deck)
                                                (@! @monster-discard))))
    (:= @monster-discard empty))
  (define (discard card)
    (cond
      [(equal? card curse) (<~@ @curses (cons card _))]
      [(equal? card bless) (<~@ @blesses (cons card _))]
      [else (<~@ @monster-discard (cons card _))]))
  (define (draw-modifier)
    ;; better not be empty after this…
    (when (empty? (@! @monster-modifier-deck)) (reshuffle-modifiers))
    (define card (first (@! @monster-modifier-deck)))
    (:= @monster-prev-discard (@! @modifier))
    (:= @modifier card)
    (<@ @monster-modifier-deck rest)
    (discard card))
  (define (draw-modifier* [better better-modifier])
    ;; better not be empty after this…
    (when (~> (@monster-modifier-deck) @! length (< 2)) (reshuffle-modifiers))
    (define cards (~> (@monster-modifier-deck) @! (take 2)))
    (define best (~> (cards) sep better))
    (define worst (cond
                    [(equal? best (first cards)) (second cards)]
                    [(equal? best (second cards)) (first cards)]))
    (:= @monster-prev-discard worst)
    (:= @modifier best)
    (<~@ @monster-modifier-deck (drop 2))
    (discard worst)
    (discard best))
  (define (shuffle-draw-pile)
    (:= @monster-modifier-deck (shuffle (@! @monster-modifier-deck))))
  (define (make-modifier-deck-adder @cards @deck)
    (thunk
      (unless (empty? (@! @cards))
        (define card (first (@! @cards)))
        (<@ @cards rest)
        (<~@ @deck (cons card _))
        (shuffle-draw-pile))))
  (define do-curse-monster (make-modifier-deck-adder @curses @monster-modifier-deck))
  (define do-bless-monster (make-modifier-deck-adder @blesses @monster-modifier-deck))
  (define add-or-remove-monster-group
    (match-lambda
      [`(add ,mg)
        (define next-id (add1 (apply max (map creature-id (@! @creatures)))))
        (define selection
          (~> (mg) monster-group-monsters
              (and (not empty?) (~> first monster-number))))
        (<~@ @creatures (append (list (creature next-id (monster-group* selection mg)))))]
      [`(remove ,mg)
        (<~@ @creatures (remf (flow (~> creature-v
                                        (and monster-group*?
                                             (~> monster-group*-mg (equal? mg))))) _))]))
  (define (make-creature-view k @e)
    (define make-player-or-monster-group-view
      (match-lambda
        [(cons ads (creature _ (? player?))) ((make-player-view @creatures) k @e)]
        [(cons ads (creature _ (? monster-group*?))) ((make-monster-group-view @creatures) k @e ads)]))
    (dyn-view
      ;; HACK: Combine @e with @ability-decks to register dyn-view dependency on
      ;; @ability-decks; but, the actual observable we care about is still just
      ;; @e. (We would ignore the car of the resulting pair in the match
      ;; patterns above, but we actually rely on its value to avoid a race.)
      ;; This is because make-monster-group-view creates a view that depends on
      ;; @ability-decks, but dyn-view isn't aware of this dependency.
      ;; https://github.com/Bogdanp/racket-gui-easy/issues/23
      (obs-combine cons @ability-decks @e)
      make-player-or-monster-group-view))
  ;; gui
  (render
    (window
      #:title "Frosthaven Manager"
      #:size '(800 600)
      (menu-bar
        (menu "Info"
              about-menu-item
              issue-menu-item
              feature-menu-item
              contribute-menu-item))
      (case-view @mode
        [(start)
         (vpanel (start-view #:on-level (λ:= @level)
                             #:on-player (λ:= @num-players))
                 (button "Play" (to-input-player-info @mode @creatures @num-players)))]
        [(input-player-info)
         (vpanel (player-input-views @num-players
                                     #:on-name (update-player-name @creatures)
                                     #:on-hp (update-player-max-hp @creatures))
                 (button "Next" (to-build-loot-deck @mode @creatures)))]
        [(build-loot-deck)
         (vpanel (loot-picker #:on-card (update-deck-and-num-loot-cards @loot-deck @num-loot-cards))
                 (spacer)
                 (button "Next" (to-choose-monster-db @mode)))]
        [(choose-monster-db)
         (vpanel
           (hpanel
             #:stretch '(#t #f)
             (button "Open Monster DB"
                     (thunk (init-dbs (or (get-file "Monster DB") default-monster-db)
                                      @info-db @action-db @ability-decks)))
             (button "Use Default Monster DB"
                     (thunk (init-dbs default-monster-db
                                      @info-db @action-db @ability-decks))))
           (db-view @info-db @action-db)
           (button "Next" (to-choose-monsters @mode)
                   #:enabled? (@~> @info-db (not hash-empty?))))]
        [(choose-monsters)
         (vpanel
           (multi-monster-picker
             @info-db @level
             #:on-change add-or-remove-monster-group)
           (button "Next" (to-play @mode @creatures)))]
        [(play)
         (vpanel
           (hpanel
             ;; left
             elements-view
             ;; main
             (group
               "Creatures"
               (list-view @creatures
                 #:min-size (@~> @creatures (~>> length (* 100) (list #f)))
                 #:key creature-id
                 make-creature-view))
             ;; right
             (vpanel #:stretch '(#f #t)
                     (let ([make-modifier-deck-adder-button
                             (λ (@cards do-adder text original-deck)
                               (button
                                 #:enabled? (@~> @cards (not empty?))
                                 (@~> @cards
                                      (~> length
                                          (format "~a (~a/~a)" text _ (length original-deck))))
                                 do-adder))])
                       (vpanel
                         (make-modifier-deck-adder-button
                           @curses do-curse-monster "Curse Monster" monster-curse-deck)
                         (make-modifier-deck-adder-button
                           @blesses do-bless-monster "Bless Monster" monster-bless-deck)))
                     (spacer)
                     (button (@~> @monster-modifier-deck (~>> length (format "Draw Modifier (~a)"))) draw-modifier)
                     (button "Advantage" draw-modifier*)
                     (button "Disadvantage" (thunk (draw-modifier* worse-modifier)))
                     (text (@~> @modifier (~>> (or _ "") (~a "Most Recent Modifier: "))))
                     (text (@~> @monster-prev-discard (~>> (or _ "") (~a "Previous Modifier: "))))
                     (spacer)
                     (button "Next Round" next-round #:enabled? @in-draw?)
                     (button "Draw Action(s)" draw #:enabled? (@> @in-draw? not))))
           ;; bottom
           (hpanel #:stretch '(#f #f)
                   (loot-button
                     @loot-deck @num-loot-cards @num-players
                     (@~> @creatures (filter (flow (~> creature-v player?)) _))
                     ;; valid because only enabled if loot-deck non-empty, and only
                     ;; closing if loot assigned
                     #:on-close (take-loot @loot-deck)
                     #:on-player (give-player-loot @creatures @loot-deck))
                   (level-stats @level @num-players)
                   (level-table @level)
                   (inspiration-table @num-players)))]
        [else (text "Broken")]))))
