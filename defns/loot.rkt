#lang racket

(provide
 (enum-out material-kind)
 (enum-out herb-kind)
 (enum-out random-item-type)
 (contract-out
  [struct money ([amount natural-number/c])]
  [struct material ([name material-kind?]
                    [amount (apply list/c (build-list (sub1 max-players) (const natural-number/c)))])]
  [struct herb ([name herb-kind?]
                [amount natural-number/c])]
  [loot-card? predicate/c]
  [format-loot-card (-> num-players/c (-> loot-card? string?))]
  [max-money-cards natural-number/c]
  [max-material-cards natural-number/c]
  [max-herb-cards natural-number/c]
  [max-random-item-cards natural-number/c]
  [money-deck (apply list/c (make-list max-money-cards money?))]
  [material-decks (hash/c material-kind? (apply list/c (make-list max-material-cards material?)))]
  [herb-decks (hash/c herb-kind? (apply list/c (make-list max-herb-cards herb?)))]
  [material-kinds (listof material-kind?)]
  [herb-kinds (listof herb-kind?)]))

(require
 racket/serialize
 rebellion/type/enum
 frosthaven-manager/qi
 frosthaven-manager/enum-helpers
 frosthaven-manager/defns/level)

(serializable-struct money [amount] #:transparent)

(define max-money-cards 20)

(define-serializable-enum-type material-kind
  (lumber metal hide)
  #:property-maker make-property-maker-that-displays-as-constant-names)

(define material-kinds
  (list lumber metal hide))

(serializable-struct material [name amount] #:transparent)

(define max-material-cards 8) ;; each

(define-serializable-enum-type herb-kind
  (arrowvine axenut corpsecap flamefruit rockroot snowthistle)
  #:property-maker make-property-maker-that-displays-as-constant-names)

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

(define loot-card? (or/c money? material? herb? random-item?))

(define (format-loot-card num-players)
  (define-flow (s? _n)
    (if (> 1) "s" ""))
  (match-lambda
    [(money amount) (format "~a gold coin~a" amount (s? amount))]
    [(material name (app (flow (list-ref (- num-players 2))) amount))
     (format "~a ~a~a" amount name (s? amount))]
    [(herb name amount) (format "~a ~a~a" amount name (s? amount))]
    [(== random-item) "The random item!"]))

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
