#lang racket

(provide
 (contract-out
  [struct level-info ([monster-level natural-number/c]
                      [gold natural-number/c]
                      [trap-damage natural-number/c]
                      [hazardous-terrain natural-number/c]
                      [exp natural-number/c])]
  [number-of-levels natural-number/c]
  [max-level natural-number/c]
  [level/c contract?]
  [max-players natural-number/c]
  [num-players/c contract?]
  [get-level-info (-> level/c level-info?)]
  [inspiration-reward (-> num-players/c natural-number/c)]))

(define max-players 4)

(define num-players/c (integer-in 2 max-players))

(struct level-info [monster-level gold trap-damage hazardous-terrain exp] #:transparent)

(define level-table
  (list (level-info 0 2 2 1 4)
        (level-info 1 2 3 2 6)
        (level-info 2 3 4 2 8)
        (level-info 3 3 5 2 10)
        (level-info 4 4 6 3 12)
        (level-info 5 4 7 3 14)
        (level-info 6 5 8 3 16)
        (level-info 7 6 9 4 18)))

(define number-of-levels (length level-table))

(define max-level (sub1 number-of-levels))

(define level/c (integer-in 0 max-level))

(define (get-level-info level)
  (list-ref level-table level))

(define (inspiration-reward num-players)
  (- 4 num-players))
