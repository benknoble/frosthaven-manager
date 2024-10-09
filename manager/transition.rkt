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
    (<@ (state-@ability-decks s) (update-ability-decks {~> 2> ability-decks-draw-next}))
    ;; toggle state
    (<@ (state-@in-draw? s) not)))

(module+ test
  (test-case "Draw Abilities: Cards not drawn for dead monster groups"
    (define s (make-sample-state))
    (define mg
      (~> (s archers)
          get-creature creature-v monster-group*-mg))
    ;; kill all archers
    (for ([m (monster-group-monsters mg)])
      (kill-monster s archers (monster-number m)))
    ;; fail: these cards no longer exist!
    (check-exn exn:fail? (thunk (get-ability-decks s archers)))
    ;; Draw should still succeed
    ((draw-abilities s))
    ;; fail: these cards still don't exist!
    (check-exn exn:fail? (thunk (get-ability-decks s archers)))))
