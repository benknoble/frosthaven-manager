#lang racket

(provide
  (contract-out
    [struct ability-decks ([current (or/c #f monster-ability?)]
                           [draw (listof monster-ability?)]
                           [discard (listof monster-ability?)])]
    [ability-decks-draw-next (-> ability-decks? ability-decks?)]
    [ability-decks-discard-and-maybe-shuffle (-> ability-decks? ability-decks?)]
    [update-ability-decks
      (-> (-> string? ability-decks? ability-decks?)
          (-> (hash/c string? ability-decks?)
              (hash/c string? ability-decks?)))]
    [move-top-draw-to-bottom (-> ability-decks? ability-decks?)]))

(require frosthaven-manager/curlique
         frosthaven-manager/defns
         racket/serialize)

(module+ test (require rackunit))

(serializable-struct ability-decks [current draw discard] #:transparent)

(define ability-decks-draw-next
  {~> (-< (~> ability-decks-draw (and (not empty?) first))
          (~> ability-decks-draw (switch [(not empty?) rest]))
          ability-decks-discard)
      ability-decks})

(module+ test
  (test-case "ability-decks-draw-next"
    (check-equal? (ability-decks-draw-next (ability-decks #f '(3 4) '(1 2)))
                  (ability-decks 3 '(4) '(1 2)))
    (check-equal? (ability-decks-draw-next (ability-decks #f '() '(1 2)))
                  (ability-decks #f '() '(1 2)))
    ;; /!\ assumption that current is #f, or you lose a card:
    (check-equal? (ability-decks-draw-next (ability-decks 3 '(4) '(1 2)))
                  (ability-decks 4 '() '(1 2)))))

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

(module+ test
  (require frosthaven-manager/monster-db)
  (define-values {_info abilities} (get-dbs default-monster-db))
  (define draw-pile (shuffle (hash-ref abilities "archer")))
  (test-case "ability-decks-discard-and-maybe-shuffle"
    (for/fold ([ad (ability-decks #f draw-pile empty)])
              ([_i (add1 (length draw-pile))])
      (define ad* (ability-decks-discard-and-maybe-shuffle (ability-decks-draw-next ad)))
      ;; current card after drawing and discarding is always #f
      (check-equal? (ability-decks-current ad*) #f)
      (cond
        ;; discarding when draw is empty triggers shuffle
        [(= 1 (length (ability-decks-draw ad)))
         ;; |draw| = 1: after drawing, draw pile is empty
         (check-true (not (empty? (ability-decks-draw ad*))))
         (check-equal? (length (ability-decks-draw ad*))
                       (length draw-pile))
         (check-true (empty? (ability-decks-discard ad*)))]
        ;; when forced to shuffle, shuffle
        [(monster-ability-shuffle? (first (ability-decks-draw ad)))
         (check-true (not (empty? (ability-decks-draw ad*))))
         (check-equal? (length (ability-decks-draw ad*))
                       (length draw-pile))
         (check-true (empty? (ability-decks-discard ad*)))]
        ;; when draw pile wasn't empty and card didn't mandate shuffle,
        ;; drawn card should go in discard
        [else
         (check-not-false (member (first (ability-decks-draw ad))
                                  (ability-decks-discard ad*)))
         (check-equal? (+ (length (ability-decks-draw ad*))
                          (length (ability-decks-discard ad*)))
                       (length draw-pile))])
      ad*)
    (void)))

(define ((update-ability-decks f) ads)
  (for/fold ([result ads])
            ([set (in-hash-keys ads)])
    (hash-update result set (λ (ad) (f set ad)))))

(define (move-top-draw-to-bottom ads)
  (define the-draw (ability-decks-draw ads))
  (define new-draw
    (match the-draw
      ['() '()]
      [(cons top rest) (append rest (list top))]))
  (struct-copy ability-decks ads [draw new-draw]))
