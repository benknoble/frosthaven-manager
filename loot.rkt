#lang racket

(provide (contract-out
           [loot-picker (-> (obs/c (listof (or/c money? material? herb? random-item?)))
                            (is-a?/c view<%>))]))

(require racket/gui/easy
         racket/gui/easy/operator
         racket/gui/easy/contract
         "defns.rkt")

(define (loot-picker @loot-deck)
  (define (make-cards-picker! label max-cards deck in-deck?)
    (define/obs @n 0)
    (obs-observe! @n
      (λ (amount)
        (<~ @loot-deck
            (λ (old-loot-deck)
              (shuffle
                (append (filter-not in-deck? old-loot-deck)
                        (take (shuffle deck) amount)))))))
    (define view (hpanel (button "-" (thunk (if (obs-peek (~> @n zero?))
                                              (void)
                                              (<~ @n sub1))))
                         (text (~> @n (λ (n) (~a label n))))
                         (button "+" (thunk (if (>= (obs-peek @n) max-cards)
                                              (void)
                                              (<~ @n add1))))))
    view)
  (define money-view
    (make-cards-picker! "Money Cards: " max-money-cards money-deck money?))
  (define material-views
    (for/list ([m (in-list material-kinds)])
      (make-cards-picker! (~a m " Cards: ")
                          max-material-cards
                          (hash-ref material-decks m)
                          (match-lambda
                            [(material (== m) _) #t]
                            [_ #f]))))
  (define herb-views
    (for/list ([h (in-list herb-kinds)])
      (make-cards-picker! (~a h " Cards: ")
                          max-herb-cards
                          (hash-ref herb-decks h)
                          (match-lambda
                            [(herb (== h)) #t]
                            [_ #f]))))
  (define/obs @random-item? #f)
  (obs-observe! @random-item?
    (λ (rand-item?)
      (<~ @loot-deck
          (λ (old-loot-deck)
            (define deck-sans-random
              (filter-not random-item? old-loot-deck))
            (if rand-item?
              (shuffle
                (cons random-item deck-sans-random))
              deck-sans-random)))))
  (define random-item-view
    (checkbox #:label "Random Item Card?"
              (λ:= @random-item? identity)))
  (define loot-view
    (apply vpanel
           random-item-view
           money-view
           (append material-views herb-views)))
  loot-view)

(module+ main
  (define/obs @loot-deck empty)
  (void (render (window
                  (hpanel
                    (loot-picker @loot-deck)
                    (table '("Card")
                           (~> @loot-deck list->vector)
                           #:entry->row (compose1 vector ~a)
                           #:min-size '(250 #f)))))))
