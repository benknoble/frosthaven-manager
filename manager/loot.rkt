#lang racket

(provide
  (contract-out
    [update-loot-deck-and-num-loot-cards
      (-> state? (-> (list/c (or/c 'add 'remove) loot-type/c) any))]
    [build-loot-deck (-> (hash/c loot-type/c natural-number/c)
                         (hash/c loot-type/c (listof loot-card?))
                         (listof loot-card?))]
    [build-loot-deck! (-> state? any)]
    [give-player-loot (-> state? (-> any/c any))]
    [place-loot-on-bottom (-> state? any)]
    [player->rewards (-> player? num-players/c level/c
                         (listof string?))]))

(require frosthaven-manager/observable-operator
         frosthaven-manager/defns
         frosthaven-manager/manager/state)

(module+ test (require rackunit))

(define ((update-loot-deck-and-num-loot-cards s) evt)
  ((loot-picker-updater (state-@type->number-of-cards s)) evt)
  (<@ (state-@num-loot-cards s) (case (car evt) [(add) add1] [(remove) sub1])))

;; valid: only called if loot-deck non-empty, loot assigned
(define (take-loot s)
  (<@ (state-@loot-deck s) {(if empty? _ rest)}))

(define ((give-player-loot* s) p)
  (define card
    (@! (@> (state-@loot-deck s) {(and (not empty?) first)})))
  (if card
    ((player-add-loot card) p)
    p))

(define ((give-player-loot s) k)
  (<@ (state-@creatures s) {(update-players k (give-player-loot* s))})
  (take-loot s))

(module+ test
  (require frosthaven-manager/testfiles/data)
  (test-case "give-player-loot"
    (define s (make-sample-state))
    (:= (state-@loot-deck s)
        (list
         (money 2)
         (material lumber '(2 2 1))
         (herb axenut 1)))
    ((give-player-loot s) jack) ;; 2 money => Jack
    ((give-player-loot s) frigg) ;; 2 lumber => Frigg
    ((give-player-loot s) jack) ;; 1 axenut => Jack
    (define cs (@! (state-@creatures s)))
    (define (check-loot-equal? player-id expected)
      (void
       (update-players cs
                       player-id
                       (λ (p)
                         (check-equal? (player-loot p) expected)
                         p))))
    (check-loot-equal? jack (list (herb axenut 1) (money 2)))
    (check-loot-equal? frigg (list (material lumber '(2 2 1))))))

(define-flow rotate
  (if empty?
    _
    (~> (-< rest first) (== _ list) append)))

(module+ test
  (test-case "rotate"
    (check-equal? (rotate '(a b c)) '(b c a))
    (check-equal? (rotate '()) '())))

(define (place-loot-on-bottom s)
  (<@ (state-@loot-deck s) rotate))

(define ((loot-picker-updater @type->number-of-cards) evt)
  (define (update cards-per-loot-deck)
    (match evt
      [`(add ,type) (hash-update cards-per-loot-deck type add1 0)]
      [`(remove ,type) (hash-update cards-per-loot-deck type sub1 0)]))
  (<@ @type->number-of-cards update))

(define (build-loot-deck type->number-of-cards type->deck)
  (shuffle
   (flatten
    (append
     (hash-ref type->deck 'special '())
     (for/list ([(type count) (in-hash type->number-of-cards)]
                #:unless (equal? type 'special))
       (define deck (hash-ref type->deck type))
       (take (shuffle deck) count))))))

(define (build-loot-deck! s)
  (:= (state-@loot-deck s)
      (build-loot-deck (@! (state-@type->number-of-cards s))
                       (@! (state-@type->deck s)))))

(define (player->rewards p num-players level)
  (define gold-factor (level-info-gold (get-level-info level)))
  (define (find-materials m)
    {(and material? (~> material-name (equal? m)))})
  (define (find-herbs h)
    {(and herb? (~> herb-name (equal? h)))})
  (define loots (player-loot p))
  (map ~a
       (apply list
              (player-name p)
              (if (memf random-item? loots) "x" "")
              (player-xp p)
              (for/sum ([loot (in-list loots)] #:when (money? loot))
                (* (money-amount loot) gold-factor))
              (append
               (for/list ([m material-kinds])
                 (for/sum ([loot (in-list (filter (find-materials m) loots))])
                   (material-amount* loot num-players)))
               (for/list ([h herb-kinds])
                 (for/sum ([loot (in-list (filter (find-herbs h) loots))])
                   (herb-amount loot)))
               (list (string-join (filter-map {(and special-loot? special-loot-name)}
                                              loots)
                                  ", "))))))
