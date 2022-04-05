#lang racket

(provide (contract-out
           [struct @player-input ([id any/c]
                                  [@name (obs/c string?)]
                                  [@hp (obs/c positive-integer?)])]
           [player-input-views (-> (obs/c (integer-in 1 max-players))
                                   (values (obs/c (listof @player-input?))
                                           (is-a?/c view<%>)))]
           [player-view (-> (obs/c player?) (obs/c initiative?)
                            (is-a?/c view<%>))]))

(require racket/gui/easy
         racket/gui/easy/operator
         racket/gui/easy/contract
         "defns.rkt")

(define (player-input-views @num-players)
  (define @ps (~> @num-players (λ (np)
                                 (map (thunk* (make-@player-input)) (range np)))))
  (define v
    (list-view @ps
               #:key @player-input-id
               (match-lambda**
                 [(id (app obs-peek (@player-input id @name @hp)))
                  (player-input-view @name @hp)])))
  (values @ps v))

(define (player-view @player @initiative)
  (define (make-condition-checkbox c)
    (checkbox #:label (~a c)
              #:checked? (~> @player (afflicted-by? c))
              (match-lambda
                [#f (<~ @player (remove-condition c))]
                [#t (<~ @player (add-condition c))])))
  (define hp-panel
    (hpanel (button "-" (thunk (if (obs-peek (~> @player dead?))
                                 (void)
                                 (<~ @player (act-on-hp sub1)))))
            (text (~> @player
                      (match-lambda
                        [(struct* player ([max-hp max] [current-hp current]))
                         (~a "HP: " current "/" max)])))
            (button "+" (thunk (if (obs-peek (~> @player at-max-health?))
                                 (void)
                                 (<~ @player (act-on-hp add1)))))))
  (define xp-panel
    (hpanel (button "-" (thunk (if (obs-peek (~> @player (compose1 zero? player-xp)))
                                 (void)
                                 (<~ @player (act-on-xp sub1)))))
            (text (~> @player (compose1 (curry ~a "XP: ") player-xp)))
            (button "+" (thunk (<~ @player (act-on-xp add1))))))
  (define name-hp-xp
    (vpanel #:alignment '(center center)
            #:stretch '(#f #t)
            (text (~> @player player-name))
            hp-panel
            xp-panel))
  (define initiative-panel
    (vpanel #:style '(border)
            #:stretch '(#f #t)
            (text (~> @initiative ~a))
            (button "Initiative"
                    (thunk
                      (render
                        (dialog (slider @initiative (λ:= @initiative identity)
                                        #:min-value 0
                                        #:max-value 99
                                        #:label (~> @player (λ (p) (~a "Initiative for " (player-name p)))))))))))
  (define conditions-panel
    (vpanel (text (~> @player (compose1 (curryr string-join ", " #:before-last " and ")
                                        (curry map ~a)
                                        player-conditions)))
            (button "Conditions"
                    (thunk
                      (render
                        (apply dialog
                               #:title (~> @player (λ (p) (~a "Conditions for " (player-name p))))
                               (map make-condition-checkbox conditions)))))))
  ;; final view
  (hpanel #:alignment '(center center)
          #:style '(border)
          initiative-panel
          name-hp-xp
          conditions-panel))

(define (player-input-view @name @hp)
  (define input-view
    (hpanel
      (input #:label "Name" @name
             (match-lambda**
               [(_ (? string? s)) (:= @name s)]
               [(_ _) (void)]))
      (button "-" (thunk (if (obs-peek (~> @hp (curry = 1)))
                           (void)
                           (<~ @hp sub1))))
      (text (~> @hp ~a))
      (button "+" (thunk (<~ @hp add1)))))
  input-view)

(struct @player-input [id @name @hp] #:transparent)
(define (make-@player-input)
  (define/obs @name "")
  (define/obs @hp 1)
  (@player-input (gensym) @name @hp))

(module+ main
  (define-values (@ps i-view) (player-input-views (@ 3)))
  (render (window
            (hpanel
              (button "Input views"
                      (thunk
                        (render (window (vpanel i-view
                                                (button "Debug" (thunk (displayln @ps))))))))
              (button "Player views"
                      (thunk
                        (render
                          (window
                            (vpanel (player-view (@ (player "A" 15 10 3 (list regenerate invisible immobilize)))
                                                 (@ 23))
                                    (spacer)
                                    (player-view (@ (player "B" 20 20 0 (list brittle)))
                                                 (@ 57))
                                    (spacer)
                                    (player-view (@ (player "C" 8 0 5 empty))
                                                 (@ 99)) )))))))))
