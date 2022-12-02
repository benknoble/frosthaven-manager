#lang racket

(provide
  (contract-out
    [update-loot-deck-and-num-loot-cards
      (-> state? (-> (list/c (or/c 'add 'remove) (listof loot-card?)) any))]
    [take-loot (-> state? (-> any))]
    [give-player-loot (-> state? (-> any/c any))]))

(require frosthaven-manager/observable-operator
         frosthaven-manager/defns
         frosthaven-manager/manager/state
         frosthaven-manager/gui/loot-picker)

(define ((update-loot-deck-and-num-loot-cards s) evt)
  ((loot-picker-updater (state-@cards-per-deck s)) evt)
  (<@ (state-@num-loot-cards s) (case (car evt) [(add) add1] [(remove) sub1])))

;; valid: only called if loot-deck non-empty, loot assigned
(define ((take-loot s)) (<@ (state-@loot-deck s) rest))

(define ((give-player-loot* s) p)
  (define card
    (@! (@~> (state-@loot-deck s) (and (not empty?) first))))
  (if card
    ((player-add-loot card) p)
    p))

(define ((give-player-loot s) k)
  (<~@ (state-@creatures s) (update-players k (give-player-loot* s))))
