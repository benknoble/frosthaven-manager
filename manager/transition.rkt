#lang racket

(provide
 (contract-out
  [transition/c contract?]
  [next-round transition/c]
  [draw-abilities transition/c]))

(require frosthaven-manager/observable-operator
         frosthaven-manager/defns
         frosthaven-manager/manager/state
         frosthaven-manager/manager/ability-decks
         frosthaven-manager/manager/modifier-decks
         frosthaven-manager/manager/elements
         frosthaven-manager/manager/round-prompts
         frosthaven-manager/gui/round-prompts)

(module+ test (require rackunit
                       frosthaven-manager/testfiles/data
                       (submod frosthaven-manager/manager/state test-helpers)))

(define transition/c (-> state? (-> any)))

;; Play (Draw -> Next Round -> …)

(define ((next-round s))
  (when (@! (state-@in-draw? s))
    ;; check prompts
    (let ([t end-of]
          [round (@! (state-@round s))])
      (when (should-do-prompt? t round (@! (state-@prompts s)))
        (do-round-prompt t round)))
    ;; wane elements
    (for-each {(<@ wane-element)} (state-@elements s))
    ;; reset player initiative
    (<@ (state-@creatures s) {(update-all-players player-clear-initiative)})
    ;; discard monster cards
    (<@ (state-@ability-decks s)
        (update-ability-decks {~> 2> ability-decks-discard-and-maybe-shuffle}))
    ;; shuffle modifiers if required
    (when (shuffle-modifier-deck? (@! (state-@monster-discard s)))
      (reshuffle-modifier-deck s))
    ;; increment round number
    (<@ (state-@round s) add1)
    ;; toggle state
    (<@ (state-@in-draw? s) not)
    ;; check prompts
    (let ([t beginning-of]
          [round (@! (state-@round s))])
      (when (should-do-prompt? t round (@! (state-@prompts s)))
        (do-round-prompt t round)))))

(define ((draw-abilities s))
  (unless (@! (state-@in-draw? s))
    ;; draw new monster cards
    (<@ (state-@ability-decks s)
        (update-ability-decks
         (λ (set ad)
           ;; TODO: if we keep only ability-decks for groups with monsters, this
           ;; can simplify?
           (define monster-set-has-monsters?
             (~>> (s) state-@active-monster-groups @! sep
                  (pass (~> monster-group-set-name (equal? set)))
                  (any (~> monster-group-monsters (not empty?)))))
           (cond
             [monster-set-has-monsters? (ability-decks-draw-next ad)]
             [else ad]))))
    ;; toggle state
    (<@ (state-@in-draw? s) not)))

(module+ test
  (test-case "Draw Abilities: Cards not drawn for dead monster groups"
    (define s (make-sample-state))
    (define initial-deck (get-ability-decks s archers))
    ;; kill all archers
    (<@ (state-@creatures s)
        {(update-monster-groups
          archers
          (λ (mg)
            (for/fold ([mg mg])
                      ([m (monster-group-monsters mg)])
              ((monster-group-remove (monster-number m)) mg))))})
    ((draw-abilities s))
    (check-equal? (get-ability-decks s archers) initial-deck)))
