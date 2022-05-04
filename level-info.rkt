#lang racket

(provide
  (contract-out
    [level-stats (-> (obs/c (integer-in 0 number-of-levels))
                     (obs/c natural-number/c)
                     (is-a?/c view<%>))]
    [level-table (-> (obs/c (integer-in 0 number-of-levels))
                     (is-a?/c view<%>))]
    [inspiration-table (-> (obs/c (integer-in 1 max-players))
                           (is-a?/c view<%>))]))

(require racket/gui/easy
         racket/gui/easy/contract
         "observable-operator.rkt"
         "defns.rkt"
         "gui/static-table.rkt")

(define (level-stats @level @num-players)
  (define @level-info (@> @level get-level-info))
  (vpanel
    (hpanel
      #:stretch '(#f #f)
      (text (@~> @level-info (~>> level-info-trap-damage (~a "Trap: "))))
      (text (@~> @level-info (~>> level-info-hazardous-terrain
                                  (~a "Hazardous Terrain: ")))))
    (hpanel
      #:stretch '(#f #f)
      (text (@~> @level-info (~>> level-info-gold (~a "Gold: "))))
      (text (@~> @level-info (~>> level-info-exp (~a "Bonus XP: "))))
      (text (@~> @num-players (~>> inspiration-reward (~a "Inspiration: ")))))))

(define (level-table @level)
  (button "Level Table"
          (thunk
            (render
              (window
                #:title "Level Information"
                #:stretch '(#f #f)
                (static-table
                  '("Level" "Gold" "Bonus XP" "Trap Damage" "Hazardous Terrain Damage")
                  number-of-levels
                  (list level-info-gold
                        level-info-exp
                        level-info-trap-damage
                        level-info-hazardous-terrain)
                  #:index->value get-level-info
                  #:selection @level))))))

(define (inspiration-table @num-players)
  (button "Inspiration Table"
          (thunk
            (render
              (window
                #:title "Inspiration"
                #:stretch '(#f #f)
                (static-table
                  '("Players" "Inspiration")
                  max-players
                  (list inspiration-reward)
                  #:index->entry add1
                  #:selection (@> @num-players sub1)))))))
