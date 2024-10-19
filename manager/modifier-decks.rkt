#lang racket

(provide
  (contract-out
    [reshuffle-modifier-deck (-> state? any)]
    [discard (-> state? monster-modifier? any)]
    [draw-modifier (-> state? (-> any))]
    [draw-modifier* (->* (state?)
                         ((-> monster-modifier? monster-modifier? monster-modifier?))
                         (-> any))]
    [do-curse-monster (-> state? (-> any))]
    [do-bless-monster (-> state? (-> any))]
    [do-bless-player (-> state? (-> any))]
    [do-unbless-player (-> state? (-> any))]
    [add-monster-modifier (-> state? (-> monster-modifier? any))]
    [remove-monster-modifier (-> state? (-> exact-nonnegative-integer? any))]))

(require frosthaven-manager/defns
         frosthaven-manager/manager/state
         frosthaven-manager/observable-operator
         frosthaven-manager/qi/utils)

(module+ test (require rackunit)
  (define (counter . xs)
    (for/fold ([h (hash)])
              ([x xs])
      (hash-update h x add1 0)))
  (define count-deck (apply counter monster-modifier-deck))
  (define (check-deck-and-discard-make-complete-deck s)
    (define all-cards
      (append (@! (state-@monster-modifier-deck s))
              (@! (state-@monster-discard s))))
    (define count-state
      (apply counter (filter {(not (or (equal? curse) (equal? bless)))}
                             all-cards)))
    (check-equal? count-state count-deck)))

(define (reshuffle-modifier-deck s)
  (define-values (@deck @discard) (on (s) (-< state-@monster-modifier-deck state-@monster-discard)))
  (define discard (@! @discard))
  (:= @discard empty)
  (<@ @deck (λ (d) (shuffle (append d discard)))))

(module+ test
  (test-case "reshuffle-modifier-deck"
    (define s (make-state))
    (check-equal? (length (@! (state-@monster-modifier-deck s))) (length monster-modifier-deck))
    (define top-3 (take (@! (state-@monster-modifier-deck s)) 3))
    (<@ (state-@monster-modifier-deck s) {(drop 3)})
    (:= (state-@monster-discard s) (reverse top-3))
    (reshuffle-modifier-deck s)
    (check-equal? (length (@! (state-@monster-modifier-deck s))) (length monster-modifier-deck))
    (check-true (empty? (@! (state-@monster-discard s))))
    (check-deck-and-discard-make-complete-deck s)))

(define (discard s card)
  (<@ (switch (card s) (% 1> 2>)
        [(equal? curse) state-@curses]
        [(equal? bless) state-@blesses]
        [else state-@monster-discard])
      {(cons card _)}))

;; only modifies state-@monster-modifier-deck
(define (draw-card s)
  ;; better not be empty after reshuffling…
  (when (empty? (@! (state-@monster-modifier-deck s)))
    (reshuffle-modifier-deck s))
  (define card (first (@! (state-@monster-modifier-deck s))))
  (<@ (state-@monster-modifier-deck s) rest)
  card)

;; only modifies state-@monster-modifier-deck
(define (draw-cards s [n 1])
  (for/list ([_ (in-range n)])
    (draw-card s)))

(define ((draw-modifier s))
  (match-define (list card) (draw-cards s 1))
  (:= (state-@monster-prev-discard s) (@! (state-@modifier s)))
  (:= (state-@modifier s) card)
  (discard s card))

(module+ test
  (test-case "draw-modifier"
    (define s (make-state))
    (define top-card (first (@! (state-@monster-modifier-deck s))))
    ((draw-modifier s))
    (check-deck-and-discard-make-complete-deck s)
    (check-equal? (@! (state-@modifier s)) top-card)

    (define top-5 (cons top-card (take (@! (state-@monster-modifier-deck s)) 4)))
    (for ([_i 4])
      ((draw-modifier s)))
    (check-deck-and-discard-make-complete-deck s)
    (check-equal? (length (@! (state-@monster-modifier-deck s))) 15)
    (check-equal? (length (@! (state-@monster-discard s))) 5)
    (check-equal? (apply counter (@! (state-@monster-discard s)))
                  (apply counter top-5))
    (check-equal? (@! (state-@modifier s)) (fifth top-5))
    (check-equal? (@! (state-@monster-prev-discard s)) (fourth top-5))

    (for ([_i 15])
      ((draw-modifier s)))
    (check-deck-and-discard-make-complete-deck s)
    (check-equal? (length (@! (state-@monster-modifier-deck s))) 0)
    (check-equal? (length (@! (state-@monster-discard s))) 20)

    ((draw-modifier s))
    (check-deck-and-discard-make-complete-deck s)
    (check-equal? (length (@! (state-@monster-modifier-deck s))) 19)
    (check-equal? (length (@! (state-@monster-discard s))) 1)))

(define ((draw-modifier* s [keep better-modifier]))
  (match-define (list a b) (draw-cards s 2))
  (define keep-card (keep a b))
  (define not-keep-card
    (match* (a b)
      [{(== keep-card) b} b]
      [{a (== keep-card)} a]))
  (:= (state-@monster-prev-discard s) not-keep-card)
  (:= (state-@modifier s) keep-card)
  (discard s not-keep-card)
  (discard s keep-card))

(module+ test
  (test-case "draw-modifier*"
    (define s (make-state))
    (define best-card (better-modifier (first (@! (state-@monster-modifier-deck s)))
                                       (second (@! (state-@monster-modifier-deck s)))))
    ((draw-modifier* s))
    (check-deck-and-discard-make-complete-deck s)
    (check-equal? (@! (state-@modifier s)) best-card)
    (check-not-false (@! (state-@monster-prev-discard s)))

    (define worst-card (worse-modifier (first (@! (state-@monster-modifier-deck s)))
                                       (second (@! (state-@monster-modifier-deck s)))))
    ((draw-modifier* s worse-modifier))
    (check-deck-and-discard-make-complete-deck s)
    (check-equal? (@! (state-@modifier s)) worst-card)
    (check-not-false (@! (state-@monster-prev-discard s)))))

(define (shuffle-modifier-deck s)
  (<@ (state-@monster-modifier-deck s) shuffle))

(define (((deck-adder state->@cards) s))
  (define @cards (state->@cards s))
  (<@ @cards (match-lambda
               [(cons card cards) (<@ (state-@monster-modifier-deck s) {(cons card _)})
                                  (shuffle-modifier-deck s)
                                  cards]
               ['() '()])))

(define do-curse-monster (deck-adder state-@curses))
(define do-bless-monster (deck-adder state-@blesses))

(define ((do-bless-player s))
  (define @cards (state-@blesses s))
  (<@ @cards (match-lambda
               [(cons card cards) (<@ (state-@player-blesses s) {(cons card _)})
                                  cards]
               ['() '()])))

(define ((do-unbless-player s))
  (define @cards (state-@player-blesses s))
  (<@ @cards (match-lambda
               [(cons card cards) (discard s card)
                                  cards]
               ['() '()])))

(module+ test
  (test-case "draw-when-cursed-blessed"
    (define s (make-state))
    (define ((add-card cards))
      (<@ (cards s)
          (match-lambda
            [(cons card cards) (<@ (state-@monster-modifier-deck s) {(cons card _)})
                               cards])))
    (define curse! (add-card state-@curses))
    (define bless! (add-card state-@blesses))

    (for ([curse-or-bless! (list curse! bless!)]
          [card (list curse bless)]
          [discard (list state-@curses state-@blesses)]
          [original (list monster-curse-deck bless-deck)])

      (curse-or-bless!)
      ((draw-modifier s))
      (check-equal? (@! (state-@modifier s)) card)
      (check-equal? (@! (discard s)) original)

      (curse-or-bless!)
      ((draw-modifier* s))
      (check-equal? (@! (discard s)) original)

      (curse-or-bless!)
      ((draw-modifier* s worse-modifier))
      (check-equal? (@! (discard s)) original)))

  (test-case "draw-when-cursed-blessed-2"
    (define s (make-state))
    (for ([_i (in-range (max (length monster-curse-deck) (length bless-deck)))])
      ((do-bless-monster s))
      ((do-curse-monster s)))
    (for ([_i (in-range (length (@! (state-@monster-modifier-deck s))))])
      ((draw-modifier s)))
    (check-equal? (@! (state-@curses s)) monster-curse-deck)
    (check-equal? (@! (state-@blesses s)) bless-deck)
    (check-deck-and-discard-make-complete-deck s)))

(define ((add-monster-modifier s) card)
  (<@ (state-@monster-modifier-deck s) {(append (list card))}))

(define ((remove-monster-modifier s) index)
  (define-values {@deck @discard} (on (s) (-< state-@monster-modifier-deck state-@monster-discard)))
  (define deck (@! @deck))
  (define L (length deck))
  (define-values {@to-change change-index}
    (cond
      [(< -1 index L) (values @deck index)]
      [else (values @discard (- index L))]))
  (<@ @to-change {~> (list-remove change-index) 1>}))

(module+ test
  (test-case "remove-monster-modifier"
    (define s (make-state))
    (define t (remove-monster-modifier s))
    (for ([_i 5])
      ((draw-modifier s)))
    (let ([expected-deck (~> (s) state-@monster-modifier-deck @! (list-remove 12) 1>)]
          [expected-discard (@! (state-@monster-discard s))])
      (t 12)
      (check-equal? (@! (state-@monster-modifier-deck s)) expected-deck)
      (check-equal? (@! (state-@monster-discard s)) expected-discard))
    (let ([expected-deck (@! (state-@monster-modifier-deck s))]
          [expected-discard (~> (s) state-@monster-discard @! (list-remove 3) 1>)])
      (t (sub1 18)) ;; normally the 18th, but we just removed one above!
      (check-equal? (@! (state-@monster-modifier-deck s)) expected-deck)
      (check-equal? (@! (state-@monster-discard s)) expected-discard))))
