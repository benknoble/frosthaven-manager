#lang racket

(provide
  state-deserialize-info
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
                   [@player-blesses (obs/c (listof monster-modifier?))]
                   [@curses (obs/c (listof monster-modifier?))]
                   [@blesses (obs/c (listof monster-modifier?))]
                   [@modifier (obs/c (or/c #f monster-modifier? ))]
                   [@monster-prev-discard (obs/c (or/c #f monster-modifier?))]
                   [@info-db (obs/c info-db/c)]
                   [@ability-db (obs/c ability-db/c)]
                   [@ability-decks (obs/c (hash/c string? ability-decks?))])]
    [make-state
      (->* ()
           ((maybe-obs/c symbol?)
            (maybe-obs/c level/c)
            (maybe-obs/c num-players/c)
            (maybe-obs/c (listof creature?))
            (maybe-obs/c (hash/c (listof loot-card?) natural-number/c))
            (maybe-obs/c (listof loot-card?))
            (maybe-obs/c natural-number/c)
            (listof (maybe-obs/c element-state/c))
            (maybe-obs/c boolean?)
            (maybe-obs/c natural-number/c)
            (maybe-obs/c (listof monster-modifier?))
            (maybe-obs/c (listof monster-modifier?))
            (maybe-obs/c (listof monster-modifier?))
            (maybe-obs/c (listof monster-modifier?))
            (maybe-obs/c (listof monster-modifier?))
            (maybe-obs/c (or/c #f monster-modifier?))
            (maybe-obs/c (or/c #f monster-modifier?))
            (maybe-obs/c info-db/c)
            (maybe-obs/c ability-db/c)
            (maybe-obs/c (hash/c string? ability-decks?)))
           state?)]
    [state-@env (-> state? (obs/c env/c))]
    [serialize-state (-> state? output-port? void?)]
    [deserialize-state (-> input-port? state?)]
    [copy-state (-> state? state? any)]
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
    [add-or-remove-monster-group (-> state? (-> (or/c add-monster-event/c remove-monster-event/c) any))]
    [draw-new-card-mid-round-if-needed (-> state? string? any)]))

(require racket/serialize
         racket/fasl
         racket/gui/easy/contract
         racket/gui/easy/observable
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
         frosthaven-manager/manager/ability-decks
         frosthaven-manager/parsers/formula)

(serializable-struct creature [id v] #:transparent)
(serializable-struct monster-group* [active mg] #:transparent)

;; private observable utilities for this module
(define-flow s@->v (~> struct->vector (vector-drop 1) (vector-map @!* _)))
(define (@!* o)
  (cond
    [(list? o) (map @! o)]
    [else (@! o)]))

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
         @player-blesses
         @curses
         @blesses
         @modifier
         @monster-prev-discard
         @info-db
         @ability-db
         @ability-decks]
        #:transparent ;; for struct->vector
        #:property prop:serializable
        (make-serialize-info
          s@->v
          #'state-deserialize-info
          #f
          (or (current-load-relative-directory) (current-directory))))

(define (make-state [@mode (@ 'start)]
                    [@level (@ 0)]
                    [@num-players (@ 2)]
                    [@creatures (@ empty)]
                    [@cards-per-deck (@ (hash))]
                    [@loot-deck (@ empty)]
                    [@num-loot-cards (@ 0)]
                    [@elements (make-states '(fire ice air earth light dark))]
                    [@in-draw? (@ #f)]
                    [@round (@ 1)]
                    [@monster-modifier-deck (@ (shuffle monster-modifier-deck))]
                    [@monster-discard (@ empty)]
                    [@player-blesses (@ empty)]
                    [@curses (@ monster-curse-deck)]
                    [@blesses (@ bless-deck)]
                    [@modifier (@ #f)]
                    [@monster-prev-discard (@ #f)]
                    [@info-db (@ (hash))]
                    [@ability-db (@ (hash))]
                    [@ability-decks (@ (hash))])
  (state (@ @mode)
         (@ @level)
         (@ @num-players)
         (@ @creatures)
         (@ @cards-per-deck)
         (@ @loot-deck)
         (@ @num-loot-cards)
         (map @ @elements)
         (@ @in-draw?)
         (@ @round)
         (@ @monster-modifier-deck)
         (@ @monster-discard)
         (@ @player-blesses)
         (@ @curses)
         (@ @blesses)
         (@ @modifier)
         (@ @monster-prev-discard)
         (@ @info-db)
         (@ @ability-db)
         (@ @ability-decks)))

(define (state-@env s)
  (obs-combine (λ (c l) (hash "C" c "L" l))
               (state-@num-players s)
               (state-@level s)))

(define state-deserialize-info
  (make-deserialize-info
    make-state
    (thunk
      (error 'state "cycles not supported"))))

(define (serialize-state s out)
  (s-exp->fasl (serialize s) out))

(define (deserialize-state in)
  (deserialize (fasl->s-exp in)))

(module+ test
  (require rackunit)
  ;; dynamic-require: break module cycle
  ;; frosthaven-manager/manager/db -> frosthaven-manager/manager/state
  (define init-dbs (dynamic-require 'frosthaven-manager/manager/db 'init-dbs))
  ;; frosthaven-manager/manager/modifier-decks -> frosthaven-manager/manager/state
  (define do-bless-player (dynamic-require 'frosthaven-manager/manager/modifier-decks 'do-bless-player))

  (test-case
    "Serializable state"

    (define s (make-state))
    (void (init-dbs default-monster-db s))
    (define-values (readable writable) (make-pipe (expt 2 10)))
    (thread (thunk (serialize-state s writable)))
    (define s* (deserialize-state readable))

    (check-equal? (s@->v s*) (s@->v s)))

  (test-case
    "Copyable state"

    (define s1 (make-state))
    (define s2 (make-state))
    (void (init-dbs default-monster-db s1)
          ((do-bless-player s1))
          ;;
          (copy-state s1 s2))

    (check-equal? (s@->v s2) (s@->v s1))))

(define (copy-state from to)
  (:=     (state-@mode to)
      (@! (state-@mode from)))
  (:=     (state-@level to)
      (@! (state-@level from)))
  (:=     (state-@num-players to)
      (@! (state-@num-players from)))
  (:=     (state-@creatures to)
      (@! (state-@creatures from)))
  (:=     (state-@cards-per-deck to)
      (@! (state-@cards-per-deck from)))
  (:=     (state-@loot-deck to)
      (@! (state-@loot-deck from)))
  (:=     (state-@num-loot-cards to)
      (@! (state-@num-loot-cards from)))
  (for ([to-element (in-list (state-@elements to))]
        [from-element (in-list (state-@elements from))])
    (:=     to-element
        (@! from-element)))
  (:=     (state-@in-draw? to)
      (@! (state-@in-draw? from)))
  (:=     (state-@round to)
      (@! (state-@round from)))
  (:=     (state-@monster-modifier-deck to)
      (@! (state-@monster-modifier-deck from)))
  (:=     (state-@monster-discard to)
      (@! (state-@monster-discard from)))
  (:=     (state-@player-blesses to)
      (@! (state-@player-blesses from)))
  (:=     (state-@curses to)
      (@! (state-@curses from)))
  (:=     (state-@blesses to)
      (@! (state-@blesses from)))
  (:=     (state-@modifier to)
      (@! (state-@modifier from)))
  (:=     (state-@monster-prev-discard to)
      (@! (state-@monster-prev-discard from)))
  (:=     (state-@info-db to)
      (@! (state-@info-db from)))
  (:=     (state-@ability-db to)
      (@! (state-@ability-db from)))
  (:=     (state-@ability-decks to)
      (@! (state-@ability-decks from))))

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

(define (draw-new-card-mid-round-if-needed s set)
  ;; mid-round, we've added a monster, and they didn't already have a card
  ;; caller must check "added a monster" condition, which varies
  (when (and (@! (state-@in-draw? s))
             (@! (@~> (state-@ability-decks s)
                      (~> (hash-ref set) (not ability-decks-current)))))
    (<~@ (state-@ability-decks s) (hash-update set ability-decks-draw-next))
    (<~@ (state-@creatures s) (sort < #:key (creature-initiative s)))))
