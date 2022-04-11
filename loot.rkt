#lang racket

(provide (contract-out
           [loot-picker (->* ()
                             (#:on-card (-> (or/c (list/c 'add (listof (or/c money? material? herb? random-item?)))
                                                  (list/c 'remove predicate/c))
                                            any))
                             (is-a?/c view<%>))]
           [loot-picker-updater
             (-> (obs/c (listof (or/c money? material? herb? random-item?)))
                 (-> (or/c (list/c 'add (listof (or/c money? material? herb? random-item?)))
                           (list/c 'remove predicate/c))
                     any))]))

(require racket/gui/easy
         "observable-operator.rkt"
         racket/gui/easy/contract
         "defns.rkt")

(define (loot-picker #:on-card [on-card void])
  (define (make-cards-picker! label max-cards deck in-deck?)
    (define/obs @n 0)
    (hpanel (spacer)
            (button "-" (thunk (if (@! (@> @n zero?))
                                 (void)
                                 (begin
                                   (<@ @n sub1)
                                   (on-card `(remove ,in-deck?))))))
            (text (@~> @n (~a label _)))
            (button "+" (thunk (if (>= (@! @n) max-cards)
                                 (void)
                                 (begin
                                   (<@ @n add1)
                                   (on-card `(add ,deck))))))
            (spacer)))
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
  (define random-item-view
    (checkbox #:label "Random Item Card?"
              (match-lambda
                [#t (on-card `(add ,(list random-item)))]
                [#f (on-card `(remove ,random-item?))])))
  (apply vpanel
         random-item-view
         money-view
         (append material-views herb-views)))

(define ((loot-picker-updater @loot-deck) evt)
  (define (update-old-deck old-loot-deck)
    (match evt
      [`(add ,from-deck) (cons (car (shuffle from-deck))
                               old-loot-deck)]
      [`(remove ,in-deck?) (remf in-deck? old-loot-deck)]))
  (<~@ @loot-deck (~> update-old-deck shuffle)))

(module+ main
  (define/obs @loot-deck empty)
  (void (render (window
                  (hpanel
                    (loot-picker #:on-card (loot-picker-updater @loot-deck))
                    (table '("Card")
                           (@> @loot-deck list->vector)
                           #:entry->row (compose1 vector ~a)
                           #:min-size '(250 #f)))))))
