#lang racket

(provide
  (contract-out
    [init-dbs (-> path-string? state? any)]
    [init-dbs-and-foes (-> path-string? state? any)]))

(require frosthaven-manager/observable-operator
         frosthaven-manager/monster-db
         frosthaven-manager/manager/state
         frosthaven-manager/manager/ability-decks)

(define (init-dbs db s)
  (define-values (info-db ability-db) (get-dbs db))
  (:= (state-@info-db s) info-db)
  (:= (state-@ability-db s) ability-db)
  (:= (state-@ability-decks s)
      (for/hash ([(set abilities) (in-hash ability-db)])
        (values set (ability-decks #f (shuffle abilities) empty)))))

(define (init-foes db s)
  (define make-foes (dynamic-require db 'make-foes (const #f)))
  (when make-foes
    ;; remove all monster groups from creatures
    (<~@ (state-@creatures s) (remf* creature-is-mg*? _))
    (define mgs (make-foes (@! (state-@level s)) (@! (state-@num-players s))))
    (define events (map (λ (mg) `(add ,mg)) mgs))
    (for-each (add-or-remove-monster-group s) events)))

(define (init-dbs-and-foes db s)
  (init-dbs db s)
  (init-foes db s))
