#lang racket

(provide
  (contract-out
    [no-duplicates? (-> list? boolean?)]
    [unique/c (-> flat-contract? contract?)]
    [unique-with/c (-> (-> any/c any/c) flat-contract? contract?)]
    [vector-update! (-> (and/c vector? (not/c immutable?))
                        natural-number/c
                        (-> any/c any/c)
                        void?)])

  ;; GUI
  (contract-out
    [struct creature ([id any/c]
                      [v (or/c player? monster-group*?)])]
    [struct monster-group* ([active (or/c #f monster-number/c)]
                            [mg monster-group?])])

  ;; level info
  (contract-out
    [struct level-info ([monster-level natural-number/c]
                        [gold natural-number/c]
                        [trap-damage natural-number/c]
                        [hazardous-terrain natural-number/c]
                        [exp natural-number/c])]
    [number-of-levels natural-number/c]
    [max-level natural-number/c]
    [level/c contract?]
    [max-players natural-number/c]
    [num-players/c contract?]
    [get-level-info (-> level/c level-info?)]
    [inspiration-reward (-> num-players/c natural-number/c)])

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
    [player-update-name (-> string? (-> player? player?))]
    [player-act-on-hp (-> (-> natural-number/c number?)
                          (-> player? player?))]
    [player-act-on-max-hp (-> (-> natural-number/c number?)
                              (-> player? player?))]
    [player-act-on-xp (-> (-> natural-number/c natural-number/c)
                          (-> player? player?))]
    [player-remove-condition (-> condition? (-> player? player?))]
    [player-add-condition (-> condition? (-> player? player?))]
    [player-condition-handler (-> (list/c condition? boolean?)
                                  (-> player? player?))]
    [player-afflicted-by? (-> condition? (-> player? boolean?))]
    [player-dead? (-> player? boolean?)]
    [player-at-max-health? (-> player? boolean?)]
    [player-set-initiative (-> player? initiative? player?)]
    [player-clear-initiative (-> player? player?)]
    [player-add-loot (-> loot-card? (-> player? player?))]
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
    [format-loot-card (-> num-players/c (-> loot-card? string?))]
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
    [ability? predicate/c]
    [conditions (listof condition?)]
    [monster-modifier-deck (listof monster-modifier?)]
    [shuffle-modifier-deck? (-> (listof monster-modifier?) boolean?)]
    [better-modifier (-> monster-modifier? monster-modifier? monster-modifier?)]
    [worse-modifier (-> monster-modifier? monster-modifier? monster-modifier?)]
    [monster-curse-deck (listof monster-modifier?)]
    [monster-bless-deck (listof monster-modifier?)])

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
    [struct monster-ability ([set-name string?]
                             [name string?]
                             [initiative initiative?]
                             [abilities (listof string?)]
                             [shuffle? boolean?])]
    [monster-number/c contract?]
    [struct monster ([number monster-number/c]
                     [elite? boolean?]
                     [current-hp natural-number/c]
                     [conditions (listof condition?)])]
    [struct monster-group ([set-name string?]
                           [name string?]
                           [level level/c]
                           [normal-stats monster-stats?]
                           [elite-stats monster-stats?]
                           [monsters (listof monster?)])]
    [make-monster (-> monster-info? level/c
                      monster-number/c boolean?
                      monster?)]
    [make-monster-group (-> monster-info? level/c
                            (and/c (listof (cons/c monster-number/c boolean?))
                                   (unique-with/c car any/c))
                            monster-group?)]
    [get-monster-stats (-> monster-group? monster? monster-stats?)]
    [monster-at-max-health? (-> monster? monster-stats? boolean?)]
    [monster-dead? (-> monster? boolean?)]
    [monster-group-update-num
      (-> monster-number/c
          (-> monster? monster?)
          (-> monster-group? monster-group?))]
    [monster-update-condition (-> condition? boolean?
                                  (-> monster? monster?))]
    [monster-update-hp (-> (-> number? number?)
                           (-> monster? monster?))]
    [monster-group-remove (-> monster-number/c
                              (-> monster-group? monster-group?))]
    [monster-group-add (-> monster-number/c boolean?
                           (-> monster-group? monster-group?))]
    [monster-group-first-monster (-> monster-group? (or/c #f monster-number/c))]))

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

(define (vector-update! v pos f)
  (vector-set! v pos (f (vector-ref v pos))))

(define max-players 4)
(define num-players/c (integer-in 1 max-players))

;; GUI

(struct creature [id v] #:transparent)
(struct monster-group* [active mg] #:transparent)

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
(define level/c (integer-in 0 max-level))
(define (get-level-info level)
  (list-ref level-table level))

(define (inspiration-reward num-players)
  (- 4 num-players))

;; players

(struct player [name max-hp current-hp xp conditions initiative loot] #:transparent)
(define (make-player name max-hp)
  (player name max-hp max-hp 0 empty 0 empty))

(define ((player-update-name name) p)
  (struct-copy player p [name name]))

(define ((player-act-on-hp proc) p)
  (define new-hp (proc (player-current-hp p)))
  (if (not (positive? new-hp))
    p
    (struct-copy player p [current-hp new-hp])))

(define ((player-act-on-max-hp proc) p)
  (define new-max-hp (proc (player-max-hp p)))
  (if (not (positive? new-max-hp))
    p
    (struct-copy player p [max-hp new-max-hp])))

(define ((player-act-on-xp proc) p)
  (define new-xp (proc (player-xp p)))
  (if (not (>= new-xp 0))
    p
    (struct-copy player p [xp new-xp])))

(define ((player-remove-condition c) p)
  (define new-conditions (remove* (list c) (player-conditions p)))
  (struct-copy player p [conditions new-conditions]))

(define ((player-add-condition c) p)
  (define new-conditions (cons c (remove* (list c) (player-conditions p))))
  (struct-copy player p [conditions new-conditions]))

(define player-condition-handler
  (match-lambda
    [`(,c #f) (player-remove-condition c)]
    [`(,c #t) (player-add-condition c)]))

(define ((player-afflicted-by? c) p)
  (and (member c (player-conditions p)) #t))

(define (player-dead? p)
  (zero? (player-current-hp p)))

(define (player-at-max-health? p)
  (>= (player-current-hp p) (player-max-hp p)))

(define (player-set-initiative p init)
  (struct-copy player p [initiative init]))

(define (player-clear-initiative p)
  (struct-copy player p [initiative 0]))

(define ((player-add-loot card) p)
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
(define ability? string?)
(define-enum-type element
  (fire ice air earth light dark)
  #:property-maker make-property-maker-that-displays-as-constant-names)
(define-enum-type monster-modifier
  (zero minus1 plus1 minus2 plus2 null crit curse bless)
  #:property-maker make-property-maker-that-displays-as-constant-names)
(define monster-modifier-deck
  (append (build-list 6 (const zero))
          (build-list 5 (const minus1))
          (build-list 5 (const plus1))
          (list minus2 plus2 null crit)))
(define monster-curse-deck (build-list 10 (const curse)))
(define monster-bless-deck (build-list 10 (const bless)))

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
        crit
        bless))
(define-flow (modifier-ranking mod)
  (~>> (index-of modifier-rankings)))
(define-flow (better-modifier x y)
  (~>> list (argmax modifier-ranking)))
(define-flow (worse-modifier x y)
  (~>> list (argmin modifier-ranking)))

(define-enum-type condition
  (regenerate ward invisible strengthen wound brittle bane poison immobilize disarm impair stun muddle)
  #:property-maker make-property-maker-that-displays-as-constant-names)
(define conditions
  (list regenerate ward invisible strengthen wound brittle bane poison immobilize disarm impair stun muddle))

;; monster cards

(struct monster-stats [max-hp move attack bonuses effects immunities] #:prefab)
(struct monster-info [set-name name normal-stats elite-stats] #:prefab)
(struct monster-ability [set-name name initiative abilities shuffle?] #:prefab)
(define monster-number/c (integer-in 1 10))
(struct monster [number elite? current-hp conditions] #:transparent)
(struct monster-group [set-name name level normal-stats elite-stats monsters] #:transparent)

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
    level
    normal elite
    (sort-monsters
      (map (match-lambda
             [(cons num elite?)
              (make-monster* (if elite? elite normal) num elite?)])
           num+elite?s))))

(define-switch (get-monster-stats mg m)
  (% 2> 1>)
  [monster-elite? monster-group-elite-stats]
  [else monster-group-normal-stats])

(define-flow (monster-at-max-health? m stats)
  (~> (== monster-current-hp monster-stats-max-hp) >=))

(define-flow (monster-dead? m)
  (~> monster-current-hp zero?))

(define-flow (sort-monsters monsters)
  ;; two passes less efficient, but easier to reason about AND we expect most
  ;; monsters lists to be "short" (10 or less).
  (~> (sort #:key monster-number <)
      ;; Notation: use t and f for #true and #false.
      ;; truth-table for strict sort-by-monster-elite?
      ;; a | b | a is first?
      ;; --+---+------------
      ;; t | t | equal ∴ f
      ;; t | f | t
      ;; f | t | f
      ;; f | f | equal ∴ f
      ;; On xor: (xor a b) is true ⇔ a and b are different. By itself, this already
      ;; covers the first and last rows of the table. The second row is also
      ;; correct, but the third is wrong. Notice that and'ing the result with a will
      ;; produce the final truth-table (since ∀ x boolean, (and x #f) = #f and also
      ;; (and x #t) = x).
      (sort #:key monster-elite? (λ (a b) (and (xor a b) a)))))

(module+ test
  (require rackunit)
  (test-case
    "sort-monsters groups by elite? and sorts by number"
    (check-equal? (sort-monsters (list (monster 1 #f 0 empty)
                                       (monster 2 #t 0 empty)))
                  (list (monster 2 #t 0 empty) (monster 1 #f 0 empty)))
    (check-equal?
      (sort-monsters
        (list
          (monster 4 #f 0 empty)
          (monster 2 #t 0 empty)
          (monster 3 #f 0 empty)
          (monster 1 #t 0 empty)))
      (list
        (monster 1 #t 0 empty)
        (monster 2 #t 0 empty)
        (monster 3 #f 0 empty)
        (monster 4 #f 0 empty)))))

(define ((monster-group-update-num num f) group)
  ;; TODO: lenses?
  ;; TODO: should monster-group-monsters be a hash?
  ;; current contract doesn't even have uniqueness
  (define (is-num? m) (= num (monster-number m)))
  (define old-monsters (monster-group-monsters group))
  (define the-monster (findf is-num? old-monsters))
  (define new-monster (f the-monster))
  (define new-monsters
    (sort-monsters (cons new-monster (remove the-monster old-monsters))))
  (struct-copy monster-group group [monsters new-monsters]))

(define ((monster-group-remove num) group)
  (define (is-num? m) (= num (monster-number m)))
  (define old-monsters (monster-group-monsters group))
  (define the-monster (findf is-num? old-monsters))
  (define new-monsters
    (sort-monsters (remove the-monster old-monsters)))
  (struct-copy monster-group group [monsters new-monsters]))

(define ((monster-group-add num elite?) group)
  (when (~>> (group) monster-group-monsters (map monster-number) (member num))
    (raise-arguments-error 'monster-group-add
                           (format "Monster ~a already exists in group" num)
                           "num" num
                           "group" group))
  (define new-monster
    (make-monster* (if elite?
                     (monster-group-elite-stats group)
                     (monster-group-normal-stats group))
                   num
                   elite?))
  (define new-monsters
    (sort-monsters (cons new-monster (monster-group-monsters group))))
  (struct-copy monster-group group [monsters new-monsters]))

(define-flow (monster-group-first-monster mg)
  (~> monster-group-monsters
      (and (not empty?) (~> first monster-number))))

(define ((monster-update-condition c on?) m)
  (define old-conditions (monster-conditions m))
  (define new-conditions
    (if on?
      (cons c (remove* (list c) old-conditions))
      (remove* (list c) old-conditions)))
  (struct-copy monster m [conditions new-conditions]))

(define ((monster-update-hp proc) m)
  (define old-hp (monster-current-hp m))
  (define new-hp (proc old-hp))
  (if (positive? new-hp)
    (struct-copy monster m [current-hp new-hp])
    m))
