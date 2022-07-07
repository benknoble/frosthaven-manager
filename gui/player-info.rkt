#lang racket

(provide
  (contract-out
    [player-input-views (->* ((obs/c natural-number/c))
                             (#:on-name (-> natural-number/c string? any)
                              #:on-hp (-> natural-number/c
                                          (-> number? number?)
                                          any)
                              #:names (listof string?)
                              #:hps (listof positive-integer?))
                             (is-a?/c view<%>))]
    [player-view (->* ((obs/c player?)
                       (obs/c num-players/c))
                      (#:on-condition (-> (list/c condition? boolean?) any)
                       #:on-hp (-> (-> number? number?) any)
                       #:on-xp (-> (-> number? number?) any)
                       #:on-initiative (-> number? any))
                      (is-a?/c view<%>))]))

(require racket/gui/easy
         "../observable-operator.rkt"
         "../qi.rkt"
         racket/gui/easy/contract
         "../defns.rkt"
         "counter.rkt")

(define (player-input-views @num-players
                            #:on-name [on-name void]
                            #:on-hp [on-hp void]
                            #:names [names #f]
                            #:hps [hps #f])
  (define (make-input-view k _@i)
    (define-flow do-name (on-name k _))
    (define-flow do-hp (on-hp k _))
    (player-input-view
      #:on-name do-name
      #:on-hp do-hp
      #:name (if names (list-ref names k) "")
      #:hp (if hps (list-ref hps k) 1)))
  (list-view (@> @num-players range)
    #:min-size (@~> @num-players (~>> (* 40) (list #f)))
    make-input-view))

(define (player-view @player
                     @num-players
                     #:on-condition [on-condition void]
                     #:on-hp [on-hp void]
                     #:on-xp [on-xp void]
                     #:on-initiative [on-initiative void])
  (define (make-condition-checkbox c)
    (checkbox #:label (~a c)
              #:checked? (@> @player (player-afflicted-by? c))
              (flow (~>> (list c) on-condition))))
  (define (subtract-hp)
    (unless (@! (@> @player player-dead?))
      (on-hp sub1)))
  (define (add-hp)
    (unless (@! (@> @player player-at-max-health?))
      (on-hp add1)))
  (define hp-panel
    (counter (@> @player player->hp-text) add-hp subtract-hp))
  (define (subtract-xp)
    (unless (@! (@~> @player (~> player-xp zero?)))
      (on-xp sub1)))
  (define (add-xp)
    (on-xp add1))
  (define xp-panel
    (counter (@~> @player (~>> player-xp (~a "XP: "))) add-xp subtract-xp))
  (define hp-xp
    (group "Stats"
           #:alignment '(center center)
           #:stretch '(#f #t)
           hp-panel
           xp-panel))
  (define @init (@> @player player-initiative))
  (define @init-label (@~> @player (~>> player-name (~a "Initiative for "))))
  (define (show-initiative-slider)
    (render
      (dialog
        #:title @init-label
        (slider
          @init
          on-initiative
          #:min-value 0
          #:max-value 99
          #:label @init-label))))
  (define name-initiative-panel
    (group
      "Initiative"
      #:stretch '(#f #t)
      (text (@> @player player-name))
      (text (@> @init ~a))
      (button "Edit Initiative" show-initiative-slider)))
  (define (show-conditions)
    (render
      (apply dialog
             #:title (@~> @player (~>> player-name (~a "Conditions for ")))
             #:size '(200 #f)
             #:style '(close-button resize-border)
             (map make-condition-checkbox conditions))))
  (define conditions-panel
    (group "Conditions"
           (text (@~> @player (~> player-conditions
                                  (sep ~a) collect
                                  (string-join ", " #:before-last " and "))))
           (button "Edit Conditions" show-conditions)))
  (define (make-loot-list p)
    (for/list ([(loot i) (in-indexed (player-loot p))])
      (cons i loot)))
  (define (make-loot-view k @e)
    (text (@~> @e (~> cdr (format-loot-card (@! @num-players))))))
  (define (show-loot)
    (render
      (window
        #:title (@~> @player (~> player-name (~a "'s Loot")))
        #:min-size (list 200 40)
        (list-view (@> @player make-loot-list)
          #:key car
          make-loot-view
          #:min-size (@~> @player
                          (~> (-< #f (~> player-loot length (* 40)))
                              list))))))
  (define loot-panel
    (button "Show Loot" show-loot))
  ;; final view
  (group
    "Player"
    #:stretch '(#t #f)
    (hpanel #:alignment '(center center)
            #:margin '(20 0)
            name-initiative-panel
            hp-xp
            (vpanel
              conditions-panel
              loot-panel))))

(define (player-input-view
          #:on-name [on-name void]
          #:on-hp [on-hp void]
          #:name [name ""]
          #:hp [hp 1])
  (define/obs @name name)
  (define/obs @hp hp)
  (define (subtract-hp)
    (on-hp sub1)
    (unless (<= (@! @hp) 1)
      (<@ @hp sub1)))
  (define (add-hp)
    (on-hp add1)
    (<@ @hp add1))
  (hpanel
    #:stretch '(#t #f)
    (input #:label "Name" @name (flow (~> 2> on-name))
           #:min-size '(200 #f))
    (counter (@~> @hp (~a "Max HP: " _)) add-hp subtract-hp)))

(module+ main
  (define (update-players players k f)
    (map (λ (e)
           (if (eq? (car e) k)
             (cons k (f (cdr e)))
             e))
         players))
  (define/obs @players
    (list
      (cons 0 (player "A" 15 10 3 (list regenerate invisible immobilize) 23
                      (list random-item)))
      (cons 1 (player "B" 20 20 0 (list brittle) 57
                      (list (first money-deck)
                            (first (hash-ref material-decks lumber)))))
      (cons 2 (player "C" 8 0 5 empty 99 empty))))
  (define i-view
    (player-input-views
      (@> @players length)
      #:on-name (λ (k name)
                  (<~@ @players (update-players k (player-update-name name))))
      #:on-hp (λ (k f)
                (<~@ @players (update-players k (player-act-on-max-hp f))))
      #:names (map (flow (~> cdr player-name)) (@! @players))
      #:hps (map (flow (~> cdr player-max-hp)) (@! @players))))
  (void
    (render (window i-view))
    (render
      (window
        (list-view @players
          #:key car
          #:min-size (@~> @players (~>> length (* 100) (list #f)))
          (λ (k @e)
            (define (update proc)
              (<~@ @players (update-players k proc)))
            (player-view
              (@> @e cdr)
              (@> @players length)
              #:on-condition (flow (~> player-condition-handler update))
              #:on-hp (flow (~> player-act-on-hp update))
              #:on-xp (flow (~> player-act-on-xp update))
              #:on-initiative (λ (i) (update (flow (player-set-initiative i)))))))))))
