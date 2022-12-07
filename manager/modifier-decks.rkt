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
    [do-bless-monster (-> state? (-> any))]))

(require frosthaven-manager/observable-operator
         frosthaven-manager/qi
         frosthaven-manager/defns
         frosthaven-manager/manager/state)

(define (reshuffle-modifier-deck s)
  (define-values (@deck @discard) (on (s) (-< state-@monster-modifier-deck state-@monster-discard)))
  (:= @deck (shuffle (append (@! @deck) (@! @discard))))
  (:= @discard empty))

(define (discard s card)
  (<~@ (switch (card s) (% 1> 2>)
         [(equal? curse) state-@curses]
         [(equal? bless) state-@blesses]
         [else state-@monster-discard])
       (cons card _)))

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

(define (shuffle-modifier-deck s)
  (:= (state-@monster-modifier-deck s) (shuffle (@! (state-@monster-modifier-deck s)))))

(define (((deck-adder state->@cards) s))
  (define @cards (state->@cards s))
  (unless (empty? (@! @cards))
    (define card (first (@! @cards)))
    (<@ @cards rest)
    (<~@ (state-@monster-modifier-deck s) (cons card _))
    (shuffle-modifier-deck s)))

(define do-curse-monster (deck-adder state-@curses))
(define do-bless-monster (deck-adder state-@blesses))
