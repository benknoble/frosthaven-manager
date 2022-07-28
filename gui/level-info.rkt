#lang racket

(provide
  (contract-out
    [level-stats (-> (obs/c level/c)
                     (obs/c natural-number/c)
                     (is-a?/c view<%>))]
    [level-table (-> (obs/c level/c)
                     (is-a?/c view<%>))]
    [inspiration-table (-> (obs/c num-players/c)
                           (is-a?/c view<%>))]))

(require racket/gui/easy
         racket/gui/easy/contract
         "../observable-operator.rkt"
         "../defns.rkt"
         "static-table.rkt")

(define (level-stats @level @num-players)
  (define @level-info (@> @level get-level-info))
  (group
    "Level Stats"
    (hpanel
      #:stretch '(#f #f)
      (text (@~> @level-info (~>> level-info-trap-damage (~a "Trap: "))))
      (text (@~> @level-info (~>> level-info-hazardous-terrain (~a "Hazardous Terrain: "))))
      (text (@~> @level-info (~>> level-info-gold (~a "Gold: "))))
      (text (@~> @level-info (~>> level-info-exp (~a "Bonus XP: "))))
      (text (@~> @num-players (~>> inspiration-reward (~a "Inspiration: ")))))))

(define (level-table @level)
  (define table
    (static-table
      '("Level" "Gold" "Bonus XP" "Trap Damage" "Hazardous Terrain Damage")
      number-of-levels
      (list level-info-gold
            level-info-exp
            level-info-trap-damage
            level-info-hazardous-terrain)
      #:entry->value get-level-info
      #:selection @level))
  (define (action)
    (render
      (window
        #:title "Level Information"
        #:stretch '(#f #f)
        table)))
  (button "Level Table" action))

(define (inspiration-table @num-players)
  (define table
    (static-table
      '("Players" "Inspiration")
      max-players
      (list inspiration-reward)
      #:index->entry add1
      #:selection (@> @num-players sub1)))
  (define (action)
    (render
      (window
        #:title "Inspiration"
        #:stretch '(#f #f)
        table)))
  (button "Inspiration Table" action))

(module+ main
  (define/obs @level 3)
  (define/obs @num-players 2)
  (render (window (vpanel (level-stats @level @num-players)
                          (level-table @level)
                          (inspiration-table @num-players)))))
