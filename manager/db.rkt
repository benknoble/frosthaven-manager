#lang racket

(provide
  (contract-out
    [init-dbs (-> path-string? state? any)]
    [init-dbs-and-foes (-> path-string? state? any)]))

(require frosthaven-manager/observable-operator
         frosthaven-manager/manager/state)

(define (init-dbs db s)
  ;; remove all monster groups from creatures
  (<@ (state-@creatures s) {(remf* creature-is-mg*? _)})
  (:= (state-@bestiary-path s) db)
  (:= (state-@ability-decks s) (hash)))

(define (init-foes db s)
  (define make-foes (dynamic-require db 'make-foes (const #f)))
  (when make-foes
    (define mgs (make-foes (@! (state-@level s)) (@! (state-@num-players s))))
    (define events (map (λ (mg) `(add ,mg)) mgs))
    (for-each (add-or-remove-monster-group s) events)))

(define (init-dbs-and-foes db s)
  (init-dbs db s)
  (init-foes db s))
