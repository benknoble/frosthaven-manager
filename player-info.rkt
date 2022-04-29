#lang racket

(provide (contract-out
           [player-input-views (-> (obs/c (listof (cons/c natural-number/c
                                                          (obs/c player?))))
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

(define (player-input-views @players)
  (list-view @players
             #:key car
             (λ (k _@player-derived) ;; don't use this derived thing
               (define @player (cdr (list-ref (@! @players) k)))
               (player-input-view
                 (@> @player player-name)
                 (@> @player player-max-hp)
                 #:on-name
                 (λ (name)
                   (<@ @player (λ (p) (struct-copy player p [name name]))))
                 #:on-hp
                 (λ (f)
                   (<@ @player
                       (match-lambda
                         [(and p (struct* player ([max-hp hp])))
                          (define new-hp (f hp))
                          (if (not (positive? new-hp))
                            p
                            (struct-copy player p [max-hp new-hp]))])))))
             #:min-size (@~> @players (~>> length (* 40) (list #f)))))

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
  (define name-hp-xp
    (vpanel #:alignment '(center center)
            #:stretch '(#f #t)
            (text (@> @player player-name))
            hp-panel
            xp-panel))
  (define initiative-panel
    (let ([@init (@> @player player-initiative)])
      (vpanel #:style '(border)
              #:stretch '(#f #t)
              (text (@> @init ~a))
              (button
                "Initiative"
                (thunk
                  (render
                    (dialog
                      (slider
                        @init
                        on-initiative
                        #:min-value 0
                        #:max-value 99
                        #:label (@~> @player (~>> player-name (~a "Initiative for ")))))))))))
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
          initiative-panel
          name-hp-xp
          conditions-panel))

(define (player-input-view @name @hp #:on-name [on-name void] #:on-hp [on-hp void])
  (hpanel
    (input #:label "Name" @name (match-lambda** [(_ s) (on-name s)])
           #:min-size '(200 #f))
    (button "-" (thunk (on-hp sub1)))
    (text (@~> @hp (~a "Max HP: " _)))
    (button "+" (thunk (on-hp add1)))))

(module+ main
  (define (condition-handler @player)
    (match-lambda
      [`(,c #f) (<@ @player (remove-condition c))]
      [`(,c #t) (<@ @player (add-condition c))]))
  (define ((hp-handler @player) proc)
    (<@ @player (act-on-hp proc)))
  (define ((xp-handler @player) proc)
    (<@ @player (act-on-xp proc)))
  (define ((initiative-handler @player) i)
    (<~@ @player (set-initiative i)))

  (define/obs @players
    (list
      (cons 0 (@ (player "A" 15 10 3 (list regenerate invisible immobilize) 23)))
      (cons 1 (@ (player "B" 20 20 0 (list brittle) 57)))
      (cons 2 (@ (player "C" 8 0 5 empty 99)))))
  (define i-view (player-input-views @players))
  (void
    (render (window i-view))
    (render
      (window
        (apply
          vpanel
          (map (match-lambda
                 [(cons _ @p)
                  (player-view
                    @p
                    #:on-condition (condition-handler @p)
                    #:on-hp (hp-handler @p)
                    #:on-xp (xp-handler @p)
                    #:on-initiative (initiative-handler @p))])
               (@! @players)))))))
