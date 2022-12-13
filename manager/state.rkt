#lang racket

(provide
  (contract-out
    [struct creature ([id any/c]
                      [v (or/c player? monster-group*?)])]
    [struct monster-group* ([active (or/c #f monster-number/c)]
                            [mg monster-group?])]
    [struct state ([@mode (obs/c symbol?)]
                   [@level (obs/c level/c)]
                   [@num-players (obs/c num-players/c)]
                   [@creatures (obs/c (listof creature?))]
                   [@cards-per-deck (obs/c (hash/c (listof loot-card?) natural-number/c))]
                   [@loot-deck (obs/c (listof loot-card?))]
                   [@num-loot-cards (obs/c natural-number/c)]
                   [@elements (listof (obs/c element-state/c))]
                   [@in-draw? (obs/c boolean?)]
                   [@round (obs/c natural-number/c)]
                   [@monster-modifier-deck (obs/c (listof monster-modifier?))]
                   [@monster-discard (obs/c (listof monster-modifier?))]
                   [@curses (obs/c (listof monster-modifier?))]
                   [@blesses (obs/c (listof monster-modifier?))]
                   [@modifier (obs/c (or/c #f monster-modifier? ))]
                   [@monster-prev-discard (obs/c (or/c #f monster-modifier?))]
                   [@info-db (obs/c info-db/c)]
                   [@ability-db (obs/c ability-db/c)]
                   [@ability-decks (obs/c (hash/c string? ability-decks?))])]
    [make-state
      (->* ()
           (symbol?
             (obs/c level/c)
             (obs/c num-players/c)
             (obs/c (listof creature?))
             (obs/c (hash/c (listof loot-card?) natural-number/c))
             (obs/c (listof loot-card?))
             (obs/c natural-number/c)
             (listof (obs/c element-state/c))
             (obs/c boolean?)
             (obs/c natural-number/c)
             (obs/c (listof monster-modifier?))
             (obs/c (listof monster-modifier?))
             (obs/c (listof monster-modifier?))
             (obs/c (listof monster-modifier?))
             (obs/c monster-modifier?)
             (obs/c monster-modifier?)
             (obs/c info-db/c)
             (obs/c ability-db/c)
             (obs/c (hash/c string? ability-decks?)))
           state?)]
    [make-player-creature (-> any/c creature?)]
    [update-players (-> (listof creature?) any/c (-> player? player?) (listof creature?))]
    [update-monster-groups (-> (listof creature?)
                               any/c
                               (-> monster-group? monster-group?)
                               (-> (or/c #f monster-number/c) monster-group? (or/c #f monster-number/c))
                               (listof creature?))]
    [update-all-players (-> (listof creature?) (-> player? player?) (listof creature?))]
    [update-all-monster-groups (-> (listof creature?) (-> monster-group? monster-group?) (listof creature?))]
    [update-player-name (-> state? (-> any/c string? any))]
    [update-player-max-hp (-> state? (-> any/c (-> natural-number/c natural-number/c) any))]
    [creature-initiative (-> state? (-> creature? (or/c +inf.0 initiative?)))]
    [add-or-remove-monster-group (-> state? (-> (or/c add-monster-event/c remove-monster-event/c) any))]))

(require racket/gui/easy/contract
         frosthaven-manager/observable-operator
         frosthaven-manager/qi
         frosthaven-manager/defns
         (only-in frosthaven-manager/gui/elements
                  element-state/c
                  make-states)
         frosthaven-manager/monster-db
         (only-in frosthaven-manager/gui/monsters
                  add-monster-event/c
                  remove-monster-event/c)
         frosthaven-manager/manager/ability-decks)

(struct creature [id v] #:transparent)
(struct monster-group* [active mg] #:transparent)

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
         @round
         @monster-modifier-deck
         @monster-discard
         @curses
         @blesses
         @modifier
         @monster-prev-discard
         @info-db
         @ability-db
         @ability-decks])

(define (make-state [@mode (@ 'start)]
                    [@level (@ 0)]
                    [@num-players (@ 1)]
                    [@creatures (@ empty)]
                    [@cards-per-deck (@ (hash))]
                    [@loot-deck (@ empty)]
                    [@num-loot-cards (@ 0)]
                    [@elements (make-states '(fire ice air earth light dark))]
                    [@in-draw? (@ #f)]
                    [@round (@ 1)]
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
         @round
         @monster-modifier-deck
         @monster-discard
         @curses
         @blesses
         @modifier
         @monster-prev-discard
         @info-db
         @ability-db
         @ability-decks))

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
      (define next-id (~> (s) state-@creatures @! (sep creature-id) max add1))
      (define selection
        (~> (mg) monster-group-monsters
            (and (not empty?) (~> first monster-number))))
      (define c (creature next-id (monster-group* selection mg)))
      (<~@ (state-@creatures s) (append (list c)))]
    [`(remove ,mg) (<~@ (state-@creatures s) (remf (creature-is-mg~? mg) _))]))
