#lang racket

(provide
 (contract-out
  [transition/c contract?]
  [to-start transition/c]
  [to-input-player-info transition/c]
  [to-build-loot-deck transition/c]
  [to-add-prompts transition/c]
  [to-choose-monster-db transition/c]
  [to-choose-monsters-or-play transition/c]
  [to-choose-monsters transition/c]
  [to-play transition/c]
  [next-round transition/c]
  [draw-abilities transition/c]))

(require frosthaven-manager/observable-operator
         frosthaven-manager/defns
         frosthaven-manager/manager/state
         frosthaven-manager/manager/ability-decks
         frosthaven-manager/manager/modifier-decks
         frosthaven-manager/manager/elements
         frosthaven-manager/manager/loot
         frosthaven-manager/manager/round-prompts
         frosthaven-manager/gui/round-prompts)

(define transition/c (-> state? (-> any)))

;; 0. Start
;; 1. Input Player Info
;; 2. Build Loot Deck
;; 3. Add Prompts
;; 4. Choose Monster DB
;; 5. Choose Monsters (optional)
;; 6. Play (Draw -> Next Round -> …)

(define ((to-start s))
  (:= (state-@mode s) 'start))

(define ((to-input-player-info s))
  (define cs (@! (state-@creatures s)))
  (define n-players (@! (state-@num-players s)))
  (define n-cs (length (filter (flow (~> creature-v player?)) cs)))
  (cond
    [(< n-cs n-players)
     (:= (state-@creatures s)
         (cs . append . (build-list (- n-players n-cs) make-player-creature)))]
    [(> n-cs n-players)
     ;; throw away old values
     (:= (state-@creatures s)
         (build-list n-players make-player-creature))])
  (:= (state-@mode s) 'input-player-info))

(define ((to-build-loot-deck s))
  ;; give each player max-hp
  (<~@ (state-@creatures s)
       (update-all-players
        (flow (~> (-< (~> player-max-hp const player-act-on-hp) _) apply))))
  (:= (state-@mode s) 'build-loot-deck))

(define ((to-add-prompts s))
  (build-loot-deck! s)
  (:= (state-@mode s) 'add-prompts))

(define ((to-choose-monster-db s))
  (:= (state-@mode s) 'choose-monster-db))

(define ((to-choose-monsters-or-play s))
  (define-flow has-mg*? (~>> state-@creatures @! (memf creature-is-mg*?)))
  ;; note parens around switch to invoke selected transition function
  ((switch (s)
     [has-mg*? to-play]
     [else to-choose-monsters])))

(define ((to-choose-monsters s))
  (:= (state-@mode s) 'choose-monsters))

(define ((to-play s))
  (:= (state-@mode s) 'play))

(define ((next-round s))
  ;; check prompts
  (let ([t end-of]
        [round (@! (state-@round s))])
    (when (should-do-prompt? t round (@! (state-@prompts s)))
      (do-round-prompt t round)))
  ;; wane elements
  (for-each (flow (<@ wane-element)) (state-@elements s))
  ;; reset player initiative
  (<~@ (state-@creatures s) (update-all-players player-clear-initiative))
  ;; discard monster cards
  (<@ (state-@ability-decks s)
      (update-ability-decks ability-decks-discard-and-maybe-shuffle))
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
      (do-round-prompt t round))))

(define ((draw-abilities s))
  ;; draw new monster cards
  (<@ (state-@ability-decks s)
      (update-ability-decks
        (λ (ad)
          (define monster-set
            (for/or ([ability (cons (ability-decks-current ad)
                                    (append (ability-decks-draw ad)
                                            (ability-decks-discard ad)))])
              (and ability (monster-ability-set-name ability))))
          (define monster-set-has-monsters?
            (for/or ([creature (@! (state-@creatures s))]
                     #:when (creature-is-mg*? creature)
                     #:do [(define v (creature-v creature))
                           (define mg (monster-group*-mg v))]
                     #:when (~> (mg) monster-group-set-name (equal? monster-set)))
              (~> (mg) monster-group-monsters (not empty?))))
          (cond
            [monster-set-has-monsters? (ability-decks-draw-next ad)]
            [else ad]))))
  ;; toggle state
  (<@ (state-@in-draw? s) not))
