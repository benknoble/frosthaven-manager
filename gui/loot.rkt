#lang racket

(provide
  (contract-out
    [loot-picker (->* ()
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
         frosthaven-manager/qi
         racket/gui/easy/contract
         frosthaven-manager/defns
         frosthaven-manager/gui/mixins
         frosthaven-manager/gui/counter
         frosthaven-manager/gui/render)

(module+ test (require rackunit))

(define (loot-picker #:on-card [on-card void]
                     #:on-sticker [on-sticker void])
  (define (make-cards-picker! label max-cards deck)
    (define/obs @n 0)
    (define/obs @stickers 0)
    (define ((send-card event) . _flow-args)
      (on-card event))
    (define ((send-sticker event) . _flow-args)
      (on-sticker event))
    (define (subtract-card)
      (<~@ @n (switch
                [zero? _]
                [else (ε (send-card `(remove ,deck)) sub1)])))
    (define (subtract-sticker)
      (<~@ @stickers (switch
                       [zero? _]
                       [else (ε (send-sticker `(remove ,deck)) sub1)])))
    (define (add-card)
      (<~@ @n (switch
                [(>= max-cards) _]
                [else (ε (send-card `(add ,deck)) add1)])))
    (define (add-sticker)
      (<~@ @stickers (switch
                       [(>= max-cards) _]
                       [else (ε (send-sticker `(add ,deck)) add1)])))
    (hpanel (spacer)
            (counter (@~> @n (~a label _)) add-card subtract-card)
            (counter (@~> @stickers (~a "+1 Stickers: " _)) add-sticker subtract-sticker)
            (spacer)))
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
           (define @rows (obs-combine make-preview-rows @loot-deck @num-players @revealed))
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

(define (make-preview-rows loot-deck num-players revealed)
  (define-values (shown hidden)
    (match revealed
      ['all (values loot-deck empty)]
      [(? number? n) (cond
                       [(<= 0 n (length loot-deck)) (split-at loot-deck n)]
                       [else (values loot-deck empty)])]))
  (~> (shown hidden)
      (== (~> sep (>< (~> (format-loot-card num-players) vector)))
          (~> sep (>< (gen (vector "?")))))
      vector))

(module+ test
  (require frosthaven-manager/defns
           frosthaven-manager/manager/loot)
  (define loot-deck (build-loot-deck (hash money-deck 3
                                           (hash-ref material-decks lumber) 2
                                           (hash-ref material-decks hide) 2
                                           (hash-ref herb-decks axenut) 2)
                                     (hash)))
  (define n-players 3)
  (define loot-text (list->vector (map vector (map (format-loot-card n-players) loot-deck))))
  (test-case "make-preview-rows"
    (check-equal? (make-preview-rows loot-deck n-players 0)
                  (vector-map (const (vector "?")) loot-text))
    ;; check that vector-map didn't modify loot-text
    (check-equal? loot-text
                  (list->vector (map vector (map (format-loot-card n-players) loot-deck))))
    (check-equal? (make-preview-rows loot-deck n-players 5)
                  (vector-append
                   (vector-take loot-text 5)
                   (vector-map (const (vector "?")) (vector-drop loot-text 5))))
    (check-equal? (make-preview-rows loot-deck n-players 'all)
                  loot-text)
    (check-equal? (make-preview-rows loot-deck n-players (length loot-deck))
                  loot-text)
    (check-equal? (make-preview-rows loot-deck n-players (add1 (length loot-deck)))
                  loot-text)
    (check-equal? (make-preview-rows loot-deck n-players -5)
                  loot-text)))

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
