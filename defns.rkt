#lang racket

(provide
  (contract-out
    [no-duplicates? (-> list? boolean?)]
    [unique/c (-> flat-contract? contract?)]
    [unique-with/c (-> (-> any/c any/c) flat-contract? contract?)])

  ;; level info
  (contract-out
    [struct level-info ([monster-level natural-number/c]
                        [gold natural-number/c]
                        [trap-damage natural-number/c]
                        [hazardous-terrain natural-number/c]
                        [exp natural-number/c])]
    [number-of-levels natural-number/c]
    [max-level natural-number/c]
    [max-players natural-number/c]
    [get-level-info (-> (integer-in 0 max-level) level-info?)]
    [inspiration-reward (-> (integer-in 1 max-players) natural-number/c)])

  ;; players
  (contract-out
    [struct player ([name string?]
                    [max-hp positive-integer?]
                    [current-hp natural-number/c]
                    [xp natural-number/c]
                    [conditions (listof condition?)]
                    [initiative initiative?]
                    [loot (listof loot-card?)])]
    [make-player (-> string? positive-integer? player?)]
    [update-name (-> string? (-> player? player?))]
    [act-on-hp (-> (-> natural-number/c number?)
                   (-> player? player?))]
    [act-on-max-hp (-> (-> natural-number/c number?)
                       (-> player? player?))]
    [act-on-xp (-> (-> natural-number/c natural-number/c)
                   (-> player? player?))]
    [remove-condition (-> condition? (-> player? player?))]
    [add-condition (-> condition? (-> player? player?))]
    [condition-handler (-> (list/c condition? boolean?)
                           (-> player? player?))]
    [afflicted-by? (-> condition? (-> player? boolean?))]
    [dead? (-> player? boolean?)]
    [at-max-health? (-> player? boolean?)]
    [set-initiative (-> player? initiative? player?)]
    [clear-initiative (-> player? player?)]
    [add-loot (-> loot-card? (-> player? player?))]
    [player->hp-text (-> player? string?)])

  ;; loot deck
  (enum-out material-kind)
  (enum-out herb-kind)
  (singleton-out random-item)
  (contract-out
    [struct money ([amount (integer-in 1 3)])]
    [struct material ([name material-kind?]
                      [amount (apply list/c (build-list max-players (const natural-number/c)))])]
    [struct herb ([name herb-kind?])]
    [loot-card? predicate/c]
    [format-loot-card (-> (integer-in 1 max-players) (-> loot-card? string?))]
    [max-money-cards natural-number/c]
    [max-material-cards natural-number/c]
    [max-herb-cards natural-number/c]
    [max-random-item-cards natural-number/c]
    [money-deck (listof money?)]
    [material-decks (hash/c material-kind? (listof material?))]
    [herb-decks (hash/c herb-kind? (listof herb?))]
    [material-kinds (listof material-kind?)]
    [herb-kinds (listof herb-kind?)])

  ;; scenario
  (enum-out element)
  (enum-out monster-modifier)
  (enum-out condition)
  (contract-out
    [initiative? predicate/c]
    [action? predicate/c]
    [conditions (listof condition?)]
    [monster-deck (listof monster-modifier?)]
    [shuffle-modifier-deck? (-> (listof monster-modifier?) boolean?)]
    [better-modifier (-> monster-modifier? monster-modifier? monster-modifier?)]
    [worse-modifier (-> monster-modifier? monster-modifier? monster-modifier?)]
    [monster-curse-deck (listof monster-modifier?)])

  ;; monster cards
  (contract-out
    [struct monster-stats ([max-hp positive-integer?]
                           [move natural-number/c]
                           [attack natural-number/c]
                           [bonuses (listof string?)]
                           [effects (listof string?)]
                           [immunities (listof string?)])]
    [struct monster-info ([set-name string?]
                          [name string?]
                          [normal-stats (apply list/c (build-list number-of-levels (const monster-stats?)))]
                          [elite-stats (apply list/c (build-list number-of-levels (const monster-stats?)))])]
    [struct monster-action ([set-name string?]
                            [name string?]
                            [initiative initiative?]
                            [abilities (listof string?)]
                            [shuffle? boolean?])]
    [struct monster ([number (integer-in 1 10)]
                     [elite? boolean?]
                     [current-hp natural-number/c]
                     [conditions (listof condition?)])]
    [struct monster-group ([set-name string?]
                           [name string?]
                           [normal-stats monster-stats?]
                           [elite-stats monster-stats?]
                           [monsters (listof monster?)])]
    [make-monster (-> monster-info? (integer-in 0 number-of-levels)
                      (integer-in 1 10) boolean?
                      monster?)]
    [make-monster-group (-> monster-info? (integer-in 0 number-of-levels)
                            (and/c (listof (cons/c (integer-in 1 10) boolean?))
                                   (unique-with/c car any/c))
                            monster-group?)]))

(require
  rebellion/type/enum
  rebellion/type/singleton
  "qi.rkt"
  "enum-helpers.rkt")

(define-flow no-duplicates?
  (not (and check-duplicates #t)))

(define (unique/c c)
  (flat-named-contract
    'unique/c
    (and/c (listof c) no-duplicates?)))

(define (unique-with/c key c)
  (define (ctc xs)
    ((unique/c c) (map key xs)))
  (flat-named-contract
    (list 'unique-with/c (object-name key) (object-name c))
    ctc))

(define max-players 4)

;; level info

(struct level-info [monster-level gold trap-damage hazardous-terrain exp] #:transparent)

(define level-table
  (list (level-info 0 2 2 1 4)
        (level-info 1 2 3 2 6)
        (level-info 2 3 4 2 8)
        (level-info 3 3 5 2 10)
        (level-info 4 4 6 3 12)
        (level-info 5 4 7 3 14)
        (level-info 6 5 8 3 16)
        (level-info 7 6 9 4 18)))
(define number-of-levels (length level-table))
(define max-level (sub1 number-of-levels))
(define (get-level-info level)
  (list-ref level-table level))

(define (inspiration-reward num-players)
  (- 4 num-players))

;; players

(struct player [name max-hp current-hp xp conditions initiative loot] #:transparent)
(define (make-player name max-hp)
  (player name max-hp max-hp 0 empty 0 empty))

(define ((update-name name) p)
  (struct-copy player p [name name]))

(define ((act-on-hp proc) p)
  (define new-hp (proc (player-current-hp p)))
  (if (not (positive? new-hp))
    p
    (struct-copy player p [current-hp new-hp])))

(define ((act-on-max-hp proc) p)
  (define new-max-hp (proc (player-max-hp p)))
  (if (not (positive? new-max-hp))
    p
    (struct-copy player p [max-hp new-max-hp])))

(define ((act-on-xp proc) p)
  (define new-xp (proc (player-xp p)))
  (if (not (>= new-xp 0))
    p
    (struct-copy player p [xp new-xp])))

(define ((remove-condition c) p)
  (define new-conditions (remove* (list c) (player-conditions p)))
  (struct-copy player p [conditions new-conditions]))

(define ((add-condition c) p)
  (define new-conditions (cons c (remove* (list c) (player-conditions p))))
  (struct-copy player p [conditions new-conditions]))

(define condition-handler
  (match-lambda
    [`(,c #f) (remove-condition c)]
    [`(,c #t) (add-condition c)]))

(define ((afflicted-by? c) p)
  (and (member c (player-conditions p)) #t))

(define (dead? p)
  (zero? (player-current-hp p)))

(define (at-max-health? p)
  (>= (player-current-hp p) (player-max-hp p)))

(define (set-initiative p init)
  (struct-copy player p [initiative init]))

(define (clear-initiative p)
  (struct-copy player p [initiative 0]))

(define ((add-loot card) p)
  (define loot (player-loot p))
  (struct-copy player p [loot (cons card loot)]))

(define (player->hp-text p)
  (match p
    [(struct* player ([max-hp max] [current-hp current]))
      (~a "HP: " current "/" max)]))

;; loot deck

(struct money [amount] #:transparent)
(define max-money-cards 20)
(define-enum-type material-kind
  (lumber metal hide)
  #:property-maker make-property-maker-that-displays-as-constant-names)
(define material-kinds
  (list lumber metal hide))
(struct material [name amount] #:transparent)
(define max-material-cards 8) ;; each
(define-enum-type herb-kind
  (arrowvine axenut corpsecap flamefruit rockroot snowthistle)
  #:property-maker make-property-maker-that-displays-as-constant-names)
(define herb-kinds
  (list arrowvine axenut corpsecap flamefruit rockroot snowthistle))
(struct herb [name] #:transparent) ;; amount is always 1
(define max-herb-cards 2) ;; each
(define-singleton-type random-item)
(define max-random-item-cards 1)

(define loot-card? (or/c money? material? herb? random-item?))

(define (format-loot-card num-players)
  (define-flow (s? n)
    (if (> 1) "s" ""))
  (match-lambda
    [(money amount) (format "~a gold coin~a" amount (s? amount))]
    [(material name (app (flow (list-ref num-players)) amount))
     (format "~a ~a~a" amount name (s? amount))]
    [(herb name) (format "1 ~a" name)]
    [(== random-item) "The random item!"]))

;; placeholders
(define money-deck
  (build-list
    max-money-cards
    (thunk* (money (random 1 4)))))
(define (make-material-deck m)
  (build-list
    max-material-cards
    (thunk* (material m (build-list max-players (thunk* (random 1 3)))))))
(define material-decks
  (for/hash ([m (in-list material-kinds)])
    (values m (make-material-deck m))))
(define herb-decks
  (for/hash ([h (in-list herb-kinds)])
    (values h (build-list max-herb-cards (thunk* (herb h))))))

;; scenario

(define initiative? (integer-in 0 99))
(define action? string?)
(define-enum-type element
  (fire ice air earth light dark)
  #:property-maker make-property-maker-that-displays-as-constant-names)
(define-enum-type monster-modifier
  (zero minus1 plus1 minus2 plus2 null crit curse)
  #:property-maker make-property-maker-that-displays-as-constant-names)
(define monster-deck
  (append (build-list 6 (const zero))
          (build-list 5 (const minus1))
          (build-list 5 (const plus1))
          (list minus2 plus2 null crit)))
(define monster-curse-deck (build-list 10 (const curse)))

(define-flow (shuffle-modifier-deck? pulled-cards)
  (~> sep (any (one-of? null crit))))

(define modifier-rankings
  (list curse
        null
        minus2
        minus1
        zero
        plus1
        plus2
        crit))
(define-flow (modifier-ranking mod)
  (~>> (index-of modifier-rankings)))
(define-flow (better-modifier x y)
  (~>> list (argmax modifier-ranking)))
(define-flow (worse-modifier x y)
  (~>> list (argmin modifier-ranking)))

(define-enum-type condition
  (regenerate ward invisible strengthen bless wound brittle bane poison immobilize disarm impair stun muddle)
  #:property-maker make-property-maker-that-displays-as-constant-names)
(define conditions
  (list regenerate ward invisible strengthen bless wound brittle bane poison immobilize disarm impair stun muddle))

;; monster cards

(struct monster-stats [max-hp move attack bonuses effects immunities] #:prefab)
(struct monster-info [set-name name normal-stats elite-stats] #:prefab)
(struct monster-action [set-name name initiative abilities shuffle?] #:prefab)
(struct monster [number elite? current-hp conditions] #:transparent)
(struct monster-group [set-name name normal-stats elite-stats monsters] #:transparent)

(define (make-monster* stats number elite?)
  (monster number elite? (monster-stats-max-hp stats) empty))

(define (make-monster info level number elite?)
  (define level-stats
    (list-ref (if elite?
                (monster-info-elite-stats info)
                (monster-info-normal-stats info))
              level))
  (make-monster* level-stats number elite?))

(define (make-monster-group info level num+elite?s)
  (define-values (normal elite)
    (~> (info)
        (-< monster-info-normal-stats
            monster-info-elite-stats)
        (amp (list-ref level))))
  (monster-group
    (monster-info-set-name info)
    (monster-info-name info)
    normal elite
    (map (match-lambda
           [(cons num elite?)
            (make-monster* (if elite? elite normal) num elite?)])
         num+elite?s)))
