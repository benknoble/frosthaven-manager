#lang racket

(provide
  ;; level info
  (contract-out
    [struct level-info ([monster-level natural-number/c]
                        [gold natural-number/c]
                        [trap-damage natural-number/c]
                        [hazardous-terrain natural-number/c]
                        [exp natural-number/c])]
    [number-of-levels natural-number/c]
    [max-players natural-number/c]
    [get-level-info (-> (integer-in 0 number-of-levels) level-info?)]
    [inspiration-reward (-> (integer-in 1 max-players) natural-number/c)])

  ;; players
  (contract-out
    [struct player ([name string?]
                    [max-hp positive-integer?]
                    [current-hp natural-number/c]
                    [xp natural-number/c]
                    [conditions (listof condition?)])]
    [make-player (-> string? positive-integer? player?)]
    [act-on-hp (-> (-> natural-number/c natural-number/c)
                   (-> player? player?))]
    [act-on-xp (-> (-> natural-number/c natural-number/c)
                   (-> player? player?))]
    [remove-condition (-> condition? (-> player? player?))]
    [add-condition (-> condition? (-> player? player?))]
    [afflicted-by? (-> condition? (-> player? boolean?))]
    [dead? (-> player? boolean?)]
    [at-max-health? (-> player? boolean?)])

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
    [max-money-cards natural-number/c]
    [max-material-cards natural-number/c]
    [max-herb-cards natural-number/c]
    [max-random-item-cards natural-number/c])

  ;; scenario
  (enum-out element)
  (enum-out monster-modifier)
  (enum-out condition)
  (contract-out
    [initiative? predicate/c]
    [action? predicate/c]
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
    [struct monster (;; monster-stats
                     [max-hp positive-integer?]
                     [move natural-number/c]
                     [attack natural-number/c]
                     [bonuses (listof string?)]
                     [effects (listof string?)]
                     [immunities (listof string?)]
                     ;; end monster-stats
                     [number (integer-in 0 10)]
                     [elite? boolean?]
                     [level (integer-in 0 number-of-levels)]
                     [current-hp natural-number/c]
                     [conditions (listof condition?)])]
    [make-monster (-> monster-info? (integer-in 0 10) boolean? (integer-in 0 number-of-levels)
                      monster?)]))

(require
  racket/struct
  rebellion/type/enum
  rebellion/type/singleton
  qi
  "enum-helpers.rkt")

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
(define (get-level-info level)
  (list-ref level-table level))

(define (inspiration-reward num-players)
  (- 4 num-players))

;; players

(struct player [name max-hp current-hp xp conditions] #:transparent)
(define (make-player name max-hp)
  (player name max-hp max-hp 0 empty))

(define (act-on-hp proc)
  (match-lambda
    [(player name max-hp curr-hp xp conds)
     (player name max-hp (proc curr-hp) xp conds)]))

(define (act-on-xp proc)
  (match-lambda
    [(player name max-hp curr-hp xp conds)
     (player name max-hp curr-hp (proc xp) conds)]))

(define (remove-condition c)
  (match-lambda
    [(player name max-hp curr-hp xp conds)
     (player name max-hp curr-hp xp (remove* (list c) conds))]))

(define (add-condition c)
  (match-lambda
    [(player name max-hp curr-hp xp conds)
     (player name max-hp curr-hp xp (cons c (remove* (list c) conds)))]))

(define ((afflicted-by? c) p)
  (and (member c (player-conditions p)) #t))

(define (dead? p)
  (zero? (player-current-hp p)))

(define (at-max-health? p)
  (= (player-max-hp p) (player-current-hp p)))

;; loot deck

(struct money [amount] #:transparent)
(define max-money-cards 20)
(define-enum-type material-kind
  (lumber metal hide)
  #:property-maker make-property-maker-that-displays-as-constant-names)
(struct material [name amount] #:transparent)
(define max-material-cards 8) ;; each
(define-enum-type herb-kind
  (arrowvine axenut corpsecap flamefruit rockroot snowthistle)
  #:property-maker make-property-maker-that-displays-as-constant-names)
(struct herb [name] #:transparent) ;; amount is always 1
(define max-herb-cards 2) ;; each
(define-singleton-type random-item)
(define max-random-item-cards 1)

(define loot-card? (or/c money? material? herb? random-item?))

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

;; monster cards

(struct monster-stats [max-hp move attack bonuses effects immunities] #:prefab)
(struct monster-info [set-name name normal-stats elite-stats] #:prefab)
(struct monster-action [set-name name initiative abilities shuffle?] #:prefab)
(struct monster monster-stats [number elite? level current-hp conditions] #:transparent)

(define (make-monster info number elite? level)
  (define level-stats
    (list-ref (if elite?
                (monster-info-elite-stats info)
                (monster-info-normal-stats info))
              level))
  (apply monster
         (append
           (struct->list level-stats)
           (list number elite? level (monster-stats-max-hp level-stats) empty))))
