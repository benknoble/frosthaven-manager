#lang racket

(provide
  (contract-out
    [loot-picker (->* ((obs/c (hash/c loot-type/c natural-number/c))
                       (obs/c (hash/c loot-type/c (listof loot-card?))))
                      (#:on-card (-> (list/c (or/c 'add 'remove) loot-type/c) any)
                       #:on-deck (-> (hash/c loot-type/c (listof loot-card?)) any))
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
         frosthaven-manager/files
         frosthaven-manager/gui/mixins
         frosthaven-manager/gui/counter
         frosthaven-manager/gui/helpers
         frosthaven-manager/gui/render
         frosthaven-manager/gui/table
         frosthaven-manager/gui/rich-text-display
         frosthaven-manager/qi/list2hash)

(define (loot-picker @type->cards @type->deck
                     #:on-card [on-card void]
                     #:on-deck [on-deck void])
  (hpanel
   (loot-cards-loader @type->deck #:on-deck on-deck)
   (base-loot-picker @type->cards #:on-card on-card)))

(define (base-loot-picker @type->cards #:on-card [on-card void])
  (define cards-picker (make-cards-picker! @type->cards #:on-card on-card))
  (define money-view (cards-picker "Money Cards: " max-money-cards 'money))
  (define material-views
    (for/list ([m (in-list material-kinds)])
      (cards-picker (~a (format-material-kind m) " Cards: ") max-material-cards m)))
  (define herb-views
    (for/list ([h (in-list herb-kinds)])
      (cards-picker (~a (format-herb-kind h) " Cards: ") max-herb-cards h)))
  (define random-item-view
    (checkbox #:label "Random Item Card?"
              #:checked? (@> @type->cards {~> (hash-ref 'random-item 0) (> 0)})
              (match-lambda
                [#t (on-card `(add random-item))]
                [#f (on-card `(remove random-item))])))
  (vpanel
    #:stretch '(#f #f)
    (text "Loot Cards in the Loot Deck")
    random-item-view
    money-view
    (apply group "Materials" material-views)
    (apply group "Herbs" herb-views)))

(define ((make-cards-picker! @type->cards #:on-card on-card)
         label max-cards type)
  (define @n (@> @type->cards {(hash-ref type 0)}))
  (define (subtract-card)
    (when (> (@! @n) 0)
      (on-card `(remove ,type))))
  (define (add-card)
    (when (< (@! @n) max-cards)
      (on-card `(add ,type))))
  (hpanel (spacer) (counter (@> @n {(~a label _)}) add-card subtract-card) (spacer)))

(define (loot-cards-loader @type->deck #:on-deck [on-deck void])
  (define-error-text @error-text with-error-text)
  (define (load-standard-cards)
    (:= @error-text "")
    (on-deck standard-loot-deck))
  (define (load-cards)
    (with-error-text
     (define file
       (get-file/filter "Loot Cards File" '("Loot Cards" "*.rkt")))
     (when file
       (define loot-cards (dynamic-require file 'loot-cards))
       (on-deck loot-cards))))
  (define (make-table-entries type->deck)
    (for*/vector ([(type deck) (in-hash type->deck)]
                  [card (in-list deck)])
      (cons type card)))
  (define (make-columns type+card)
    (match-define (cons _type card) type+card)
    (for/vector ([num-players '(2 3 4)])
      ((format-loot-card num-players) card)))
  (vpanel
   (text "Useable Loot Cards")
   (table
    '("2 players" "3 players" "4 players")
    (@> @type->deck make-table-entries)
    (λ (action entries selection)
      (case action
        [(dclick)
         (when (and selection (exact-nonnegative-integer? selection))
           (match-define (cons type card) (vector-ref entries selection))
           (unless (memq type '(random-item special))
             (define new-card (apply-sticker card))
             (on-deck
              (~> (entries) vector->list
                  (list-set selection (cons type new-card))
                  (group-by car _)
                  (list~>hash #:->key (~> car car)
                              #:->value (map cdr _))))))]))
    #:entry->row make-columns)
   (vpanel
    #:stretch '(#f #f)
    (hpanel (button "Use Standard Loot Cards" load-standard-cards)
            (button "Load Loot Cards" load-cards))
    (cond-view
      [(@> @error-text non-empty-string?)
       (hpanel (text "Error Message:" #:color "red")
               (rich-text-display (@> @error-text {~> (string-split "\n") (add-between newline)})
                                  #:min-size '(#f 60)))]
      [else (spacer)]))))

(define (loot-button @loot-deck
                     @num-loot-cards
                     @num-players
                     @players
                     #:on-player [on-player void]
                     #:on-top [on-top void]
                     #:on-bottom [on-bottom void])
  (define-flow (loot-text _deck _num-cards)
    (~>> (== length (or _ 0)) (format "Loot (~a/~a)!")))
  (define (show-assigner)
    ;; not setting current renderer, nor using an eventspace: dialog
    (render (loot-assigner @loot-deck @num-players @players on-player on-top on-bottom)))
  (button (obs-combine loot-text @loot-deck @num-loot-cards)
          #:enabled? (@> @loot-deck {(not empty?)})
          show-assigner))

(define (loot-assigner @loot-deck @num-players @players on-player on-top on-bottom)
  (define-close! close! closing-mixin)
  (define mixin closing-mixin)
  (define/match (make-player-button _e)
    [{(cons p id)}
      (define (action)
        (on-player id)
        (close!))
      (button (player-name p) action)])
  (define-flow (card-text _num-players _deck)
    (if (~> 2> (not empty?))
      (~> (== format-loot-card first) apply)
      ""))
  (dialog
    #:mixin mixin
    #:title "Loot card"
    #:style empty
    (text (obs-combine card-text @num-players @loot-deck))
    (observable-view @players {~> (sep make-player-button) hpanel})
    (hpanel (spacer)
            (button "Top of Deck" (thunk (on-top) (close!)))
            (button "Bottom of Deck" (thunk (on-bottom) (close!)))
            (spacer))))

(define (loot-preview @loot-deck @num-players)
  (button "Preview Loot"
          (thunk
           (define/obs @revealed 0)
           (define @rows (obs-combine preview-rows @loot-deck @num-players @revealed))
           ;; not setting current renderer, nor using an eventspace: dialog
           (render
            (dialog
             #:title "Loot Deck Previewer"
             #:size '(350 450)
             (hpanel
              (table '("Loot Card") @rows)
              (vpanel
               (button "Reveal 1" (thunk (<@ @revealed {switch [number? add1]}))
                       #:enabled? (obs-combine {~> (== _ length)
                                                   (and (~> 1> number?) <)}
                                               @revealed @loot-deck))
               (button "Reveal All" (thunk (:= @revealed 'all))
                       #:enabled? (@> @revealed number?))
               (spacer))))))))

(define (preview-rows loot-deck num-players revealed)
  (define reveal {~> (esc (format-loot-card num-players)) vector})
  (define hide-loot {(gen (vector "?"))})
  (make-preview-rows loot-deck revealed #:reveal reveal #:hide hide-loot))

(module+ main
  (require frosthaven-manager/manager)
  (define s (make-state))
  (define (table-with-actual-loot-deck)
    (define @deck (obs-combine build-loot-deck @type->cards @type->deck))
    (vpanel
      (hpanel (text "Duplicates?")
              (text (@> @deck {~>> (map eq-hash-code) check-duplicates ~a})))
      (table '("ID" "Cards")
             (@> @deck list->vector)
             #:entry->row {~> (-< eq-hash-code _) (>< ~a) vector}
             #:min-size '(250 300))))
  (define count+decks->row {~> (-< car cdr) (>< ~a) vector})
  (define @type->cards (state-@type->number-of-cards s))
  (define @type->deck (state-@type->deck s))
  (void (render/eventspace
          ;; no separate eventspace: block main until this window closed
          (window (hpanel (loot-picker @type->cards
                                       @type->deck
                                       #:on-card (update-loot-deck-and-num-loot-cards s)
                                       #:on-deck (λ:= (state-@type->deck s)))
                          (table '("Deck" "Cards")
                                 (@> @type->cards {~> hash->list list->vector})
                                 #:entry->row count+decks->row
                                 #:min-size '(250 #f))
                          (table-with-actual-loot-deck))))))
