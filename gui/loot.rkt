#lang racket

(provide
  (contract-out
    [loot-picker (->* ((obs/c (hash/c (listof loot-card?) natural-number/c))
                       (obs/c (hash/c (listof loot-card?) natural-number/c)))
                      (#:on-card (-> (list/c (or/c 'add 'remove) (listof loot-card?)) any)
                       #:on-sticker (-> (list/c (or/c 'add 'remove) (listof loot-card?)) any))
                      (is-a?/c view<%>))]
    [loot-button
      (->* ((obs/c (listof loot-card?))
            (obs/c natural-number/c)
            (obs/c num-players/c)
            (obs/c (listof (cons/c player? any/c))))
           (#:on-player (-> any/c any)
            #:on-top (-> any)
            #:on-bottom (-> any))
           (is-a?/c view<%>))]
    [loot-preview (-> (obs/c (listof loot-card?))
                      (obs/c num-players/c)
                      (is-a?/c view<%>))]))

(require racket/gui/easy
         frosthaven-manager/observable-operator
         racket/gui/easy/contract
         frosthaven-manager/defns
         frosthaven-manager/gui/mixins
         frosthaven-manager/gui/counter
         frosthaven-manager/gui/render
         frosthaven-manager/gui/table)

(define (loot-picker @cards-per-deck
                     @stickers-per-deck
                     #:on-card [on-card void]
                     #:on-sticker [on-sticker void])
  (define cards-picker (make-cards-picker! @cards-per-deck @stickers-per-deck #:on-card on-card #:on-sticker on-sticker))
  (define money-view (cards-picker "Money Cards: " max-money-cards money-deck))
  (define material-views
    (for/list ([m (in-list material-kinds)])
      (cards-picker (~a m " Cards: ") max-material-cards (hash-ref material-decks m))))
  (define herb-views
    (for/list ([h (in-list herb-kinds)])
      (cards-picker (~a h " Cards: ") max-herb-cards (hash-ref herb-decks h))))
  (define random-item-view
    (let ([deck (list random-item)])
      (checkbox #:label "Random Item Card?"
                #:checked? (@~> @cards-per-deck (~> (hash-ref deck 0) (> 0)))
                (match-lambda
                  [#t (on-card `(add ,deck))]
                  [#f (on-card `(remove ,deck))]))))
  (vpanel
    #:stretch '(#f #f)
    random-item-view
    money-view
    (apply group "Materials" material-views)
    (apply group "Herbs" herb-views)))

(define ((make-cards-picker! @cards-per-deck @stickers-per-deck #:on-card on-card #:on-sticker on-sticker)
         label max-cards deck)
  (define @n (@~> @cards-per-deck (hash-ref deck 0)))
  (define @stickers (@~> @stickers-per-deck (hash-ref deck 0)))
  (define (subtract-card)
    (when (> (@! @n) 0)
      (on-card `(remove ,deck))))
  (define (subtract-sticker)
    (when (> (@! @stickers) 0)
      (on-sticker `(remove ,deck))))
  (define (add-card)
    (when (< (@! @n) max-cards)
      (on-card `(add ,deck))))
  (define (add-sticker)
    (when (< (@! @stickers) max-cards)
      (on-sticker `(add ,deck))))
  (hpanel (spacer)
          (counter (@~> @n (~a label _)) add-card subtract-card)
          (counter (@~> @stickers (~a "+1 Stickers: " _)) add-sticker subtract-sticker)
          (spacer)))

(define (loot-button @loot-deck
                     @num-loot-cards
                     @num-players
                     @players
                     #:on-player [on-player void]
                     #:on-top [on-top void]
                     #:on-bottom [on-bottom void])
  (define-flow (loot-text deck num-cards)
    (~>> (== length (or _ 0)) (format "Loot (~a/~a)!")))
  (define (show-assigner)
    ;; not setting current renderer, nor using an eventspace: dialog
    (render (loot-assigner @loot-deck @num-players @players on-player on-top on-bottom)))
  (button (obs-combine loot-text @loot-deck @num-loot-cards)
          #:enabled? (@~> @loot-deck (not empty?))
          show-assigner))

(define (loot-assigner @loot-deck @num-players @players on-player on-top on-bottom)
  (define-close! close! closing-mixin)
  (define-flow mixin closing-mixin)
  (define/match (make-player-button e)
    [{(cons p id)}
      (define (action)
        (on-player id)
        (close!))
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
    (observable-view @players (flow (~> (sep make-player-button) hpanel)))
    (hpanel (spacer)
            (button "Top of Deck" (thunk (on-top) (close!)))
            (button "Bottom of Deck" (thunk (on-bottom) (close!)))
            (spacer))))

(define (loot-preview @loot-deck @num-players)
  (button "Preview Loot"
          (thunk
           (define/obs @revealed 0)
           (define @rows (obs-combine preview-rows @loot-deck @num-players @revealed))
           (with-closing-custodian/eventspace
            (render/eventspace
             #:eventspace closing-eventspace
             (window
              #:mixin close-custodian-mixin
              #:title "Loot Deck Previewer"
              #:size '(350 450)
              (hpanel
               (table '("Loot Card") @rows)
               (vpanel
                (button "Reveal 1" (thunk (<~@ @revealed (switch [number? add1])))
                        #:enabled? (obs-combine (flow (~> (== _ length)
                                                          (and (~> 1> number?) <)))
                                                @revealed @loot-deck))
                (button "Reveal All" (thunk (:= @revealed 'all))
                        #:enabled? (@> @revealed number?))
                (spacer)))))))))

(define (preview-rows loot-deck num-players revealed)
  (define-flow reveal (~> (format-loot-card num-players) vector))
  (define-flow hide-loot (gen (vector "?")))
  (make-preview-rows loot-deck revealed #:reveal reveal #:hide hide-loot))

(module+ main
  (require frosthaven-manager/manager)
  (define s (make-state))
  (define/match (find-deck card)
    [{(money _)} "Money"]
    [{(or (material kind _) (herb kind _))} (~a kind)]
    [{(== random-item)} "Random Item"])
  (define (table-with-actual-loot-deck)
    (define @deck (obs-combine build-loot-deck @cards-per-loot-deck (state-@stickers-per-loot-deck s)))
    ;; not setting current renderer, nor using an eventspace: dialog
    (vpanel
      (hpanel (text "Duplicates?")
              (text (@~> @deck (~>> (map eq-hash-code) check-duplicates ~a))))
      (table '("ID" "Cards")
             (@> @deck list->vector)
             #:entry->row (flow (~> (-< eq-hash-code _) (>< ~a) vector))
             #:min-size '(250 300))))
  (define-flow count+decks->row (~> (-< (~> car car find-deck) (~> cdr ~a)) vector))
  (define/obs @cards-per-loot-deck (state-@cards-per-deck s))
  (void (render/eventspace
          ;; no separate eventspace: block main until this window closed
          (window (hpanel (loot-picker #:on-card (update-loot-deck-and-num-loot-cards s)
                                       #:on-sticker (update-stickers-per-deck s))
                          (table '("Deck" "Cards")
                                 (@~> @cards-per-loot-deck (~> hash->list list->vector))
                                 #:entry->row count+decks->row
                                 #:min-size '(250 #f))
                          (table-with-actual-loot-deck))))))