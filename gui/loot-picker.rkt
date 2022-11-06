#lang racket

(provide
  (contract-out
    [loot-picker (->* ()
                      (#:on-card (-> (or/c (list/c 'add (listof loot-card?))
                                           (list/c 'remove predicate/c))
                                     any))
                      (is-a?/c view<%>))]
    [loot-picker-updater
      (-> (obs/c (listof loot-card?))
          (-> (or/c (list/c 'add (listof loot-card?))
                    (list/c 'remove predicate/c))
              any))]
    [loot-button
      (->* ((obs/c (listof loot-card?))
            (obs/c natural-number/c)
            (obs/c natural-number/c)
            (obs/c (listof creature?)))
           (#:on-close (-> any)
            #:on-player (-> any/c any))
           (is-a?/c view<%>))]))

(require racket/gui/easy
         frosthaven-manager/observable-operator
         frosthaven-manager/qi
         racket/gui/easy/contract
         frosthaven-manager/defns
         frosthaven-manager/gui/mixins
         frosthaven-manager/gui/counter
         frosthaven-manager/gui/render)

(define (loot-picker #:on-card [on-card void])
  (define (make-cards-picker! label max-cards deck in-deck?)
    (define/obs @n 0)
    (define (subtract-card)
      (unless (@! (@> @n zero?))
        (<@ @n sub1)
        (on-card `(remove ,in-deck?))))
    (define (add-card)
      (unless (>= (@! @n) max-cards)
        (<@ @n add1)
        (on-card `(add ,deck))))
    (hpanel (spacer)
            (counter (@~> @n (~a label _)) add-card subtract-card)
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
  (vpanel
    #:stretch '(#f #f)
    random-item-view
    money-view
    (apply group "Materials" material-views)
    (apply group "Herbs" herb-views)))

(define ((loot-picker-updater @loot-deck) evt)
  (define (update-old-deck old-loot-deck)
    (match evt
      [`(add ,from-deck) (cons (car (shuffle from-deck))
                               old-loot-deck)]
      [`(remove ,in-deck?) (remf in-deck? old-loot-deck)]))
  (<~@ @loot-deck (~> update-old-deck shuffle)))

(define (loot-button @loot-deck
                     @num-loot-cards
                     @num-players
                     @players
                     #:on-player [on-player void]
                     #:on-close [on-close void])
  (define-flow (loot-text deck num-cards)
    (~>> (== length (or _ 0)) (format "Loot (~a/~a)!")))
  (define (show-assigner)
    (with-closing-custodian/eventspace
      (render/eventspace #:eventspace closing-eventspace
                         (loot-assigner @loot-deck @num-players @players on-player on-close
                                        #:mixin close-custodian-mixin))))
  (button (obs-combine loot-text @loot-deck @num-loot-cards)
          #:enabled? (@~> @loot-deck (not empty?))
          show-assigner))

(define (loot-assigner @loot-deck @num-players @players on-player on-close
                       #:mixin [extra-mix values])
  (define close! (box #f))
  (define (set-close! c) (set-box! close! c))
  (define-flow mixin
    (~> (make-closing-proc-mixin set-close!)
        (make-on-close-mixin on-close)
        extra-mix))
  (define (make-player-button e)
    (define (action)
      (on-player (creature-id e))
      ((unbox close!)))
    (button (player-name (creature-v e)) action))
  (define-flow (card-text num-players deck)
    (if (~> 2> (not empty?))
      (~> (== format-loot-card first) apply)
      ""))
  (dialog
    #:mixin mixin
    #:title "Loot card"
    #:size '(250 100)
    #:style empty
    (text (obs-combine card-text @num-players @loot-deck))
    (apply hpanel
           ;; valid because only called inside a thunk, and part of a dialog;
           ;; doesn't need to react to adding/subtracting players
           (map make-player-button (@! @players)))))

(module+ main
  (define/obs @loot-deck empty)
  (void (render/eventspace
          ;; no separate eventspace: block main until this window closed
          (window (hpanel (loot-picker #:on-card (loot-picker-updater @loot-deck))
                          (table '("Card")
                                 (@> @loot-deck list->vector)
                                 #:entry->row (compose1 vector ~a)
                                 #:min-size '(250 #f)))))))
