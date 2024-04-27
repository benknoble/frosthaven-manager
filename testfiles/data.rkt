#lang racket

(provide make-sample-state
         make-sample-loot-deck
         jack
         frigg
         archers
         boss
         more-monsters)

(require racket/runtime-path
         syntax/parse/define
         frosthaven-manager/defns
         frosthaven-manager/monster-db
         frosthaven-manager/observable-operator)

;; (submod manager/loot test) uses this module; break the cycle
(define-syntax-parse-rule (define-manager-values (name:id ...))
  (define-values (name ...)
    (values (dynamic-require 'frosthaven-manager/manager 'name) ...)))

(define-manager-values
 (make-state
  init-dbs
  state-@num-players
  state-@creatures
  creature
  add-or-remove-monster-group
  update-loot-deck-and-num-loot-cards
  build-loot-deck!))

(define-runtime-path more-monsters "sample-bestiary-import.rkt")

(define-values (info _abilities) (get-dbs more-monsters))

(define mg
  (make-monster-group (~> (info) (hash-ref "archer") (hash-ref "hynox archer"))
                      0
                      '([1 . #t] [2 . #f] [3 . #t])
                      (hash)))

(define boss-mg
  (make-monster-group (~> (info) (hash-ref "boss") (hash-ref "giant squid"))
                      0
                      '([1 . #f])
                      (hash "C" 2)))

(define (make-sample-state)
  (define sample-state (make-state (@ 'play)))
  (void
   (init-dbs more-monsters sample-state)
   (:= (state-@num-players sample-state) 2)
   (:= (state-@creatures sample-state)
       (list (creature 0 (~> ((make-player "Jack Skellington" 8))
                             (player-summon "Corpse Bro" 4)))
             (creature 1 (~> ((player "Frigg" 12 10 3 (list muddle ward) 67 empty empty))
                             (player-summon "Banner of Courage" 7)))))
   ((add-or-remove-monster-group sample-state) `(add ,mg))
   ((add-or-remove-monster-group sample-state) `(add ,boss-mg)))
  sample-state)

(define jack 0)
(define frigg 1)
(define archers 2)
(define boss 3)

(define (make-sample-loot-deck s)
  (void
   (for ([type (append (list 'money)
                       (hash-keys material-decks)
                       (hash-keys herb-decks))])
     ((update-loot-deck-and-num-loot-cards s) `(add ,type)))
   (build-loot-deck! s)))
