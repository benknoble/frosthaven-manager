#lang racket

(provide (contract-out
           [loot-picker (-> (values (obs/c (listof (or/c money?
                                                         material?
                                                         herb?
                                                         random-item?)))
                                    (is-a?/c view<%>)))]))

(require racket/gui/easy
         racket/gui/easy/operator
         racket/gui/easy/contract
         "defns.rkt")

(define (loot-picker)
  (define (make-cards-picker label max-cards deck)
    (define/obs @n 0)
    (define @deck (~> @n (λ (n) (take (shuffle deck) n))))
    (define view (hpanel (button "-" (thunk (if (obs-peek (~> @n zero?))
                                              (void)
                                              (<~ @n sub1))))
                         (text (~> @n (λ (n) (~a label n))))
                         (button "+" (thunk (if (>= (obs-peek @n) max-cards)
                                              (void)
                                              (<~ @n add1))))))
    (values @deck view))
  (define-values (@money-deck money-view)
    (make-cards-picker "Money Cards: " max-money-cards money-deck))
  (define material-@d+v
    (for/list ([m (in-list material-kinds)])
      (define-values (@d v) (make-cards-picker (~a m " Cards: ") max-material-cards
                                               (hash-ref material-decks m)))
      (cons @d v)))
  (define herb-@d+v
    (for/list ([h (in-list herb-kinds)])
      (define-values (@d v) (make-cards-picker (~a h " Cards: ") max-herb-cards
                                               (hash-ref herb-decks h)))
      (cons @d v)))
  (define/obs @random-item? #f)
  (define random-item-view
    (checkbox #:label "Random Item Card?"
              (λ:= @random-item? identity)))
  (define @loot-deck
    (apply obs-combine
           (λ (money rand-item? . mats-and-herbs)
             (shuffle (apply append
                             (if rand-item? (list random-item) empty)
                             money
                             mats-and-herbs)))
           @money-deck
           @random-item?
           (append (map car material-@d+v)
                   (map car herb-@d+v))))
  (define loot-view
    (apply vpanel
           random-item-view
           money-view
           (append (map cdr material-@d+v)
                   (map cdr herb-@d+v))))
  (values @loot-deck loot-view))

(module+ main
  (define-values (@loot-deck loot-view)
    (loot-picker))
  (void (render (window
                  (hpanel
                    loot-view
                    (table '("Card")
                           (~> @loot-deck list->vector)
                           #:entry->row (compose1 vector ~a)
                           #:min-size '(250 #f)))))))
