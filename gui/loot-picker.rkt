#lang racket

(provide
  (contract-out
    [loot-picker (->* ()
                      (#:on-card (-> (list/c (or/c 'add 'remove) (listof loot-card?)) any))
                      (is-a?/c view<%>))]
    [loot-picker-updater (-> (obs/c (hash/c (listof loot-card?) natural-number/c))
                             (-> (list/c (or/c 'add 'remove) (listof loot-card?))
                                 any))]
    [build-loot-deck (-> (hash/c (listof loot-card?) natural-number/c)
                         (listof loot-card?))]
    [loot-button
      (->* ((obs/c (listof loot-card?))
            (obs/c natural-number/c)
            (obs/c natural-number/c)
            (obs/c (listof (cons/c player? any/c))))
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
  (define (make-cards-picker! label max-cards deck)
    (define/obs @n 0)
    (define ((send-event event) . _flow-args)
      (on-card event))
    (define (subtract-card)
      (<~@ @n (switch
                [zero? _]
                [else (ε (send-event `(remove ,deck)) sub1)])))
    (define (add-card)
      (<~@ @n (switch
                [(>= max-cards) _]
                [else (ε (send-event `(add ,deck)) add1)])))
    (hpanel (spacer) (counter (@~> @n (~a label _)) add-card subtract-card) (spacer)))
  (define money-view (make-cards-picker! "Money Cards: " max-money-cards money-deck))
  (define material-views
    (for/list ([m (in-list material-kinds)])
      (make-cards-picker! (~a m " Cards: ") max-material-cards (hash-ref material-decks m))))
  (define herb-views
    (for/list ([h (in-list herb-kinds)])
      (make-cards-picker! (~a h " Cards: ") max-herb-cards (hash-ref herb-decks h))))
  (define/obs @random-item? #f)
  (define random-item-view
    (let ([deck (list random-item)])
      (checkbox #:label "Random Item Card?"
                (match-lambda
                  [#t (on-card `(add ,deck))]
                  [#f (on-card `(remove ,deck))]))))
  (vpanel
    #:stretch '(#f #f)
    random-item-view
    money-view
    (apply group "Materials" material-views)
    (apply group "Herbs" herb-views)))

(define ((loot-picker-updater @cards-per-loot-deck) evt)
  (define (update cards-per-loot-deck)
    (match evt
      [`(add ,deck) (hash-update cards-per-loot-deck deck add1 0)]
      [`(remove ,deck) (hash-update cards-per-loot-deck deck sub1 0)]))
  (<@ @cards-per-loot-deck update))

(define (build-loot-deck cards-per-loot-deck)
  (shuffle (flatten (for/list ([(deck count) (in-hash cards-per-loot-deck)])
                      (take (shuffle deck) count)))))

(define (loot-button @loot-deck
                     @num-loot-cards
                     @num-players
                     @players
                     #:on-player [on-player void]
                     #:on-close [on-close void])
  (define-flow (loot-text deck num-cards)
    (~>> (== length (or _ 0)) (format "Loot (~a/~a)!")))
  (define (show-assigner)
    ;; not setting current renderer, nor using an eventspace: dialog
    (render (loot-assigner @loot-deck @num-players @players on-player on-close)))
  (button (obs-combine loot-text @loot-deck @num-loot-cards)
          #:enabled? (@~> @loot-deck (not empty?))
          show-assigner))

(define (loot-assigner @loot-deck @num-players @players on-player on-close)
  (define close! (box #f))
  (define (set-close! c) (set-box! close! c))
  (define-flow mixin
    (~> (make-closing-proc-mixin set-close!)
        (make-on-close-mixin on-close)))
  (define/match (make-player-button e)
    [{(cons p id)}
      (define (action)
        (on-player id)
        ((unbox close!)))
      (button (player-name p) action)])
  (define-flow (card-text num-players deck)
    (if (~> 2> (not empty?))
      (~> (== format-loot-card first) apply)
      ""))
  (dialog
    #:mixin mixin
    #:title "Loot card"
    #:style empty
    (text (obs-combine card-text @num-players @loot-deck))
    (observable-view @players (flow (~> (sep make-player-button) hpanel)))))

(module+ main
  (define/match (find-deck card)
    [{(money _)} "Money"]
    [{(or (material kind _) (herb kind))} (~a kind)]
    [{(== random-item)} "Random Item"])
  (define (table-with-actual-loot-deck)
    (define @deck (@> @cards-per-loot-deck build-loot-deck))
    ;; not setting current renderer, nor using an eventspace: dialog
    (vpanel
      (hpanel (text "Duplicates?")
              (text (@~> @deck (~>> (map eq-hash-code) check-duplicates ~a))))
      (table '("ID" "Cards")
             (@> @deck list->vector)
             #:entry->row (flow (~> (-< eq-hash-code _) (>< ~a) vector))
             #:min-size '(250 300))))
  (define-flow count+decks->row (~> (-< (~> car car find-deck) (~> cdr ~a)) vector))
  (define/obs @cards-per-loot-deck (hash))
  (void (render/eventspace
          ;; no separate eventspace: block main until this window closed
          (window (hpanel (loot-picker #:on-card (loot-picker-updater @cards-per-loot-deck))
                          (table '("Deck" "Cards")
                                 (@~> @cards-per-loot-deck (~> hash->list list->vector))
                                 #:entry->row count+decks->row
                                 #:min-size '(250 #f))
                          (table-with-actual-loot-deck))))))
