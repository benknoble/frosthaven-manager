#lang racket

(provide
  (contract-out
    [init-dbs (-> path-string? state? any)]))

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
