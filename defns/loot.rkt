#lang racket

(provide
 (enum-out material-kind)
 (enum-out herb-kind)
 (enum-out random-item-type)
 (contract-out
  [format-material-kind (-> material-kind? string?)]
  [parse-material-kind (-> string? material-kind?)]
  [format-herb-kind (-> herb-kind? string?)]
  [parse-herb-kind (-> string? herb-kind?)]
  [struct money ([amount natural-number/c])]
  [struct material ([name material-kind?]
                    [amount (apply list/c (build-list (sub1 max-players) (const natural-number/c)))])]
  [material-amount* (-> material? num-players/c natural-number/c)]
  [struct herb ([name herb-kind?]
                [amount natural-number/c])]
  [struct special-loot ([name string?])]
  [loot-card? (-> any/c boolean?)]
  [format-loot-card (-> num-players/c (-> loot-card? string?))]
  [max-money-cards natural-number/c]
  [max-material-cards natural-number/c]
  [max-herb-cards natural-number/c]
  [max-random-item-cards natural-number/c]
  [money-deck (apply list/c (make-list max-money-cards money?))]
  [material-decks (hash/c material-kind? (apply list/c (make-list max-material-cards material?)))]
  [herb-decks (hash/c herb-kind? (apply list/c (make-list max-herb-cards herb?)))]
  [standard-loot-deck (hash/c loot-type/c (listof loot-card?))]
  [material-kinds (listof material-kind?)]
  [herb-kinds (listof herb-kind?)]
  [apply-sticker (-> (and/c loot-card?
                            (not/c random-item?)
                            (not/c special-loot?)) loot-card?)]
  [loot-type/c flat-contract?]
  [card->type (-> loot-card? loot-type/c)]))

(require frosthaven-manager/constants
         frosthaven-manager/curlique
         frosthaven-manager/defns/level
         frosthaven-manager/enum-helpers
         racket/hash
         racket/serialize
         rebellion/type/enum)

(module+ test (require rackunit))

(serializable-struct money [amount] #:transparent)

(define max-money-cards 20)

(define-serializable-enum-type material-kind
  (lumber metal hide))

(define-constant-format/parse
 format-material-kind parse-material-kind
 ([lumber "Lumber"]
  [metal "Metal"]
  [hide "Hide"]))

(define material-kinds
  (list lumber metal hide))

(serializable-struct material [name amount] #:transparent)

(define max-material-cards 8) ;; each

(define-serializable-enum-type herb-kind
  (arrowvine axenut corpsecap flamefruit rockroot snowthistle))

(define-constant-format/parse
 format-herb-kind parse-herb-kind
 ([arrowvine "Arrowvine"]
  [axenut "Axenut"]
  [corpsecap "Corpsecap"]
  [flamefruit "Flamefruit"]
  [rockroot "Rockroot"]
  [snowthistle "Snowthistle"]))

(define herb-kinds
  (list arrowvine axenut corpsecap flamefruit rockroot snowthistle))

(serializable-struct/versions herb 1 [name amount]
                              ([0
                                (λ (name) (herb name 1))
                                (thunk (error 'herb "cycles not supported"))])
                              #:transparent)

(define max-herb-cards 2) ;; each

(define-serializable-enum-type random-item-type
  (random-item)
  #:descriptor-name discriptor:random-item
  #:predicate-name random-item?
  #:discriminator-name discriminator:random-item
  #:selector-name selector:random-item)

(define max-random-item-cards 1)

(serializable-struct special-loot [name] #:transparent)

(define loot-card? (or/c money? material? herb? random-item? special-loot?))

(define-flow (material-amount* _loot _num-players)
  (~> (== material-amount (- 2)) list-ref))

(define (format-loot-card num-players)
  (define s? {(if (> 1) "s" "")})
  (match-lambda
    [(money amount) (format "~a gold coin~a" amount (s? amount))]
    [(and (material name _) (app {(material-amount* num-players)} amount))
     (format "~a ~a~a" amount (format-material-kind name) (s? amount))]
    [(herb name amount) (format "~a ~a~a" amount (format-herb-kind name) (s? amount))]
    [(== random-item) "The random item!"]
    [(special-loot name) (format "Special Loot: ~a" name)]))

(module+ test
  (let ([F (format-loot-card 3)])
    (check-equal? (F (money 1)) "1 gold coin")
    (check-equal? (F (money 2)) "2 gold coins")
    (check-equal? (F (material metal (list 2 1 1))) "1 Metal")
    (check-equal? (F (material metal (list 2 2 1))) "2 Metals")
    (check-equal? (F (herb axenut 1)) "1 Axenut")
    (check-equal? (F (herb axenut 2)) "2 Axenuts")
    (check-equal? (F random-item) "The random item!")
    (check-equal? (F (special-loot "1418")) "Special Loot: 1418")))

(define money-deck
  (append
    (build-list 12 (thunk* (money 1)))
    (build-list 06 (thunk* (money 2)))
    (build-list 02 (thunk* (money 3)))))

(define material-decks
  (let* ([amounts '(
                    (1 1 1) (1 1 1)
                    (2 2 1) (2 2 1) (2 2 1)
                    (2 1 1) (2 1 1) (2 1 1))]
         [make-deck (λ (m) (map (λ (amount) (material m amount)) amounts))])
    (hash lumber (make-deck lumber)
          metal (make-deck metal)
          hide (make-deck hide))))

(define herb-decks
  (for/hash ([h (in-list herb-kinds)])
    (values h (build-list max-herb-cards (thunk* (herb h 1))))))

(define (apply-sticker card)
  (match card
    [(money amount) (money (add1 amount))]
    [(material name amount) (material name (map add1 amount))]
    [(herb name amount) (herb name (add1 amount))]))

(define loot-type/c
  (or/c 'money material-kind? herb-kind? 'random-item 'special))

(define (card->type c)
  (match c
    [(money _) 'money]
    [(material m _) m]
    [(herb t _) t]
    [(? random-item?) 'random-item]
    [(special-loot _) 'special]))

(define standard-loot-deck
  (hash-union (hash 'money money-deck 'random-item (list random-item))
              material-decks
              herb-decks))
