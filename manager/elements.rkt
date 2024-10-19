#lang racket

(provide
 (contract-out
  [element-state/c contract?]
  [make-states (-> (listof any/c) (listof (obs/c element-state/c)))]
  [infuse-all (-> (listof (obs/c element-state/c)) any)]
  [consume-all (-> (listof (obs/c element-state/c)) any)]
  [wane-element (-> element-state/c element-state/c)]
  [transition-element-state (-> element-state/c element-state/c)]))

(require frosthaven-manager/observable-operator
         racket/gui/easy/contract)

(module+ test (require rackunit))

(define element-state/c (or/c 'unfused 'infused 'waning))

(define (make-states es)
  ;; don't use const; we don't want them to all be eq?
  (map (λ (_) (@ 'unfused)) es))

(module+ test
  (test-case "make-states"
    (check-equal? (length (make-states (range 6))) 6)
    (check-false (let ([states (make-states (range 6))])
                   (andmap eq? (drop-right states 1) (cdr states))))))

(define ((make-all state) es)
  (for ([@e (in-list es)])
    (:= @e state)))

(define infuse-all (make-all 'infused))
(define consume-all (make-all 'unfused))

(module+ test
  (test-case "*-all"
    (define states (make-states (range 6)))
    (infuse-all states)
    (check-true (andmap {(equal? 'infused)} (map @! states)))
    (consume-all states)
    (check-true (andmap {(equal? 'unfused)} (map @! states)))))

(define wane-element
  (match-lambda
    ['infused 'waning]
    ['waning 'unfused]
    [_ 'unfused]))

(define transition-element-state
  (match-lambda
    ['unfused 'infused]
    ['infused 'waning]
    ['waning 'unfused]
    [_ 'infused]))
