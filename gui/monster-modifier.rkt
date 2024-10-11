#lang racket

(provide
 (contract-out
  [modify-monster-deck-menu-item
   (->* {(obs/c (listof monster-modifier?))}
        {#:on-add (-> monster-modifier? any)
         #:on-remove (-> exact-nonnegative-integer? any)
         #:on-shuffle (-> any)}
        (is-a?/c view<%>))]
  [favors-dialog
   (->* {(obs/c (listof monster-modifier?))}
        {#:on-add (-> monster-modifier? any)
         #:on-remove (-> exact-nonnegative-integer? any)
         #:on-shuffle (-> any)}
        (is-a?/c window-view<%>))]
  [card-swapper
   (->* {(obs/c (listof monster-modifier?))}
        {#:on-add (-> monster-modifier? any)
         #:on-remove (-> exact-nonnegative-integer? any)}
        (is-a?/c view<%>))]))

(require racket/gui/easy
         racket/gui/easy/contract
         frosthaven-manager/observable-operator
         frosthaven-manager/qi/utils

         frosthaven-manager/defns)

(define (modify-monster-deck-menu-item @monster-ability-cards
                                       #:on-add [add void]
                                       #:on-remove [remove void]
                                       #:on-shuffle [shuffle void])
  (menu-item
   "Modify Monster Deck"
   (thunk
    ;; not setting current renderer, nor using an eventspace: dialog
    (render
     (favors-dialog @monster-ability-cards
                    #:on-add add
                    #:on-remove remove
                    #:on-shuffle shuffle)))))

(define (favors-dialog @monster-ability-cards #:on-add [add void] #:on-remove [remove void] #:on-shuffle [shuffle void])
  (dialog
   #:title "Favors: Adjust Monster Modifier Deck"
   #:size '(450 500)
   #:style '(resize-border close-button)
   (vpanel
    #:alignment '(right top)
    (card-swapper @monster-ability-cards #:on-add add #:on-remove remove)
    ;; TODO: compute points spent? Only "valid" for removing minus cards, and
    ;; card-swapper permits removing any card…
    (button "Shuffle Deck" shuffle))))

(define (card-swapper @monster-ability-cards #:on-add [add void] #:on-remove [remove void])
  (define-flow moveable? (not (one-of? curse bless)))
  (define/obs @current-index #f)
  (define/obs @current #f)
  (define/obs @absent-index #f)
  (define/obs @absent #f)
  (define @absent-cards
    (@> @monster-ability-cards {~> absent-from-modifier-deck list->vector}))
  (define (action @i @c)
    (match-lambda**
      [{(or 'select 'dclick) entries (? number? index)}
       (:= @i index)
       (:= @c (vector-ref entries index))]
      [{_ _ #f}
       (:= @i #f)
       (:= @c #f)]
      [{_ _ _} (void)]))
  (define (add* card)
    ;; call add
    (add card)
    ;; update @absent, @absent-index
    (define absent-cards (@! @absent-cards))
    (define index (@! @absent-index))
    (cond
      [(>= index (vector-length absent-cards))
       (:= @absent-index #f)
       (:= @absent #f)]
      [else (:= @absent (vector-ref absent-cards index))]))
  ;; card -> (vector/c string?)
  (define make-row {~> format-monster-modifier vector})
  (hpanel
   (table '("Current Cards")
          (@> @monster-ability-cards list->vector)
          (action @current-index @current)
          #:entry->row make-row
          #:selection @current-index)
   (vpanel
    #:alignment '(center center)
    (button "=>" (thunk (cond [(@! @current-index) => remove]))
            #:enabled? (obs-combine (flow (and% moveable? values))
                                    @current
                                    @current-index))
    (button "<=" (thunk (cond [(@! @absent) => add*]))
            #:enabled? @absent))
   (table '("Out of the Game")
          @absent-cards
          (action @absent-index @absent)
          #:entry->row make-row
          #:selection @absent-index)))

(module+ main
  (define/obs @monster-ability-cards monster-modifier-deck)
  (render
   (favors-dialog
    @monster-ability-cards
    #:on-add (λ (c) (<@ @monster-ability-cards {(append (list c))}))
    #:on-remove (λ (i)
                  (<@ @monster-ability-cards {~> (list-remove i) 1>}))
    #:on-shuffle (thunk (<@ @monster-ability-cards shuffle)))))
