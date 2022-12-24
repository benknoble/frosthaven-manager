#lang racket

(provide
  (contract-out
    [struct ability-decks ([current (or/c #f monster-ability?)]
                           [draw (listof monster-ability?)]
                           [discard (listof monster-ability?)])]
    [ability-decks-draw-next (-> ability-decks? ability-decks?)]
    [ability-decks-discard-and-maybe-shuffle (-> ability-decks? ability-decks?)]
    [update-ability-decks
      (-> (-> ability-decks? ability-decks?)
          (-> (hash/c string? ability-decks?)
              (hash/c string? ability-decks?)))]))

(require racket/serialize
         frosthaven-manager/qi
         frosthaven-manager/defns)

(serializable-struct ability-decks [current draw discard] #:transparent)

(define-flow (ability-decks-draw-next ad)
  (~> (-< (~> ability-decks-draw (and (not empty?) first))
          (~> ability-decks-draw (switch [(not empty?) rest]))
          ability-decks-discard)
      ability-decks))

(define (ability-decks-discard-and-maybe-shuffle ad)
  (match-define (ability-decks current draw discard) ad)
  (define discard-with-current
    (if (monster-ability? current)
      (cons current discard)
      discard))
  (define shuffle?
    (or (empty? draw)
        (on (current)
          (and monster-ability? monster-ability-shuffle?))))
  (define-values (draw* discard*)
    (if shuffle?
      (values (shuffle (append draw discard-with-current)) empty)
      (values draw discard-with-current)))
  (ability-decks #f draw* discard*))

(define ((update-ability-decks f) ads)
  (for/hash ([(set ad) (in-hash ads)])
    (values set (f ad))))
