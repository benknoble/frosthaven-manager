#lang racket

(provide (contract-out
           [player-input-views (->* ((obs/c natural-number/c))
                                    (#:on-name (-> natural-number/c string? any)
                                     #:on-hp (-> natural-number/c
                                                 (-> number? number?)
                                                 any)
                                     #:names (listof string?)
                                     #:hps (listof positive-integer?))
                                    (is-a?/c view<%>))]
           [player-view (->* ((obs/c player?))
                             (#:on-condition (-> (list/c condition? boolean?) any)
                              #:on-hp (-> (-> number? number?) any)
                              #:on-xp (-> (-> number? number?) any)
                              #:on-initiative (-> number? any))
                             (is-a?/c view<%>))]))

(require racket/gui/easy
         "observable-operator.rkt"
         "qi.rkt"
         racket/gui/easy/contract
         "defns.rkt")

(define (player-input-views @num-players
                            #:on-name [on-name void]
                            #:on-hp [on-hp void]
                            #:names [names #f]
                            #:hps [hps #f])
  (list-view (@> @num-players range)
    #:min-size (@~> @num-players (~>> (* 40) (list #f)))
    (λ (k _@i)
      (player-input-view
        #:on-name (flow (on-name k _))
        #:on-hp (flow (on-hp k _))
        #:name (if names (list-ref names k) "")
        #:hp (if hps (list-ref hps k) 1)))))

(define (player-view @player
                     #:on-condition [on-condition void]
                     #:on-hp [on-hp void]
                     #:on-xp [on-xp void]
                     #:on-initiative [on-initiative void])
  (define (make-condition-checkbox c)
    (checkbox #:label (~a c)
              #:checked? (@> @player (afflicted-by? c))
              (flow (~>> (list c) on-condition))))
  (define hp-panel
    (hpanel (button "-" (thunk (unless (@! (@> @player dead?))
                                 (on-hp sub1))))
            (text (@> @player
                      (match-lambda
                        [(struct* player ([max-hp max] [current-hp current]))
                         (~a "HP: " current "/" max)])))
            (button "+" (thunk (unless (@! (@> @player at-max-health?))
                                 (on-hp add1))))))
  (define xp-panel
    (hpanel (button "-" (thunk (unless (@! (@~> @player (~> player-xp zero?)))
                                 (on-xp sub1))))
            (text (@~> @player (~>> player-xp (~a "XP: "))))
            (button "+" (thunk (on-xp add1)))))
  (define hp-xp
    (vpanel #:alignment '(center center)
            #:stretch '(#f #t)
            hp-panel
            xp-panel))
  (define name-initiative-panel
    (let ([@init (@> @player player-initiative)]
          [@label (@~> @player (~>> player-name (~a "Initiative for ")))])
      (vpanel #:style '(border)
              #:stretch '(#f #t)
              (text (@> @player player-name))
              (text (@> @init ~a))
              (button
                "Initiative"
                (thunk
                  (render
                    (dialog
                      #:title @label
                      (slider
                        @init
                        on-initiative
                        #:min-value 0
                        #:max-value 99
                        #:label @label))))))))
  (define conditions-panel
    (vpanel (text (@~> @player (~> player-conditions
                                   (sep ~a) collect
                                   (string-join ", " #:before-last " and "))))
            (button "Conditions"
                    (thunk
                      (render
                        (apply dialog
                               #:title (@~> @player (~>> player-name (~a "Conditions for ")))
                               (map make-condition-checkbox conditions)))))))
  ;; final view
  (hpanel #:alignment '(center center)
          #:style '(border)
          #:stretch '(#t #f)
          name-initiative-panel
          hp-xp
          conditions-panel))

(define (player-input-view
          #:on-name [on-name void]
          #:on-hp [on-hp void]
          #:name [name ""]
          #:hp [hp 1])
  (define/obs @name name)
  (define/obs @hp hp)
  (hpanel
    #:stretch '(#t #f)
    (input #:label "Name" @name (flow (~> 2> on-name))
           #:min-size '(200 #f))
    (button "-" (thunk
                  (on-hp sub1)
                  (unless (<= (@! @hp) 1)
                    (<@ @hp sub1))))
    (text (@~> @hp (~a "Max HP: " _)))
    (button "+" (thunk
                  (on-hp add1)
                  (<@ @hp add1)))))

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
                  (<~@ @players (update-players k (update-name name))))
      #:on-hp (λ (k f)
                (<~@ @players (update-players k (act-on-max-hp f))))
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
              #:on-condition (flow (~> condition-handler update))
              #:on-hp (flow (~> act-on-hp update))
              #:on-xp (flow (~> act-on-xp update))
              #:on-initiative (λ (i) (update (flow (set-initiative i)))))))))))
