#lang racket

(provide (contract-out
           [info-db/c contract?]
           [action-db/c contract?]
           [get-dbs (-> path-string? (values info-db/c action-db/c))]
           [default-monster-db path-string?]))

(require racket/runtime-path
         qi
         "defns.rkt")

(define info-db/c
  (hash/c string? (hash/c string? monster-info?)))
(define action-db/c
  (hash/c string? (listof monster-action?)))

(define-runtime-path default-monster-db "monster-db.rktd")

(define (list->hash xs #:->key [->key identity] #:->value [->value identity])
  (for/hash ([x (in-list xs)])
    (on (x) (-< ->key ->value))))

(define (get-dbs db-file)
  (~> (db-file)
      file->list sep
      (-<
        ;; info db
        (~>> (pass monster-info?) collect
             (group-by monster-info-set-name)
             (list->hash #:->key (flow (~> car monster-info-set-name))
                         #:->value (flow (list->hash #:->key monster-info-name))))
        ;; actions deck
        (~>> (pass monster-action?) collect
             (group-by monster-action-set-name)
             (list->hash #:->key (flow (~> car monster-action-set-name)))))))

(module+ gui
  (provide (contract-out
             [single-monster-picker (-> info-db/c (is-a?/c view<%>))]))

  (require racket/gui/easy
           racket/gui/easy/operator
           racket/gui/easy/operator)

  ;; TODO: monster-view
  ;; some similarities to player-view, but need room for actions and overall
  ;; stats, the monster numbers + elite? status, ability to "hide" action, etc.

  ;; TODO: how to give back state of which monsters were added?
  ;; can we just return the add-monster-states, too?
  (define (single-monster-picker info-db #|@monsters|#)
    (define sets (hash-keys info-db))
    (define/obs @set (car sets))
    (define set-picker (choice #:label "Set" sets (λ:= @set identity)))
    (define @valid-monsters (~> @set (flow (~>> (hash-ref info-db) hash-keys))))
    (define/obs @info #f)
    (define monster-picker (choice #:label "Monster" @valid-monsters (λ:= @info identity)))
    (define add-monster-states
      (for/list ([num (in-inclusive-range 1 10)])
        (list num #|elite?|# (@ #f) #|to-add?|# (@ #f))))
    (define add-monster-pickers
      (map (match-lambda [(list num @elite? @add?)
                          (hpanel
                            (checkbox (λ:= @add? identity)
                                      #:label (~a num)
                                      #:checked? @add?)
                            (checkbox (λ:= @elite? identity)
                                      #:label "Elite?"
                                      #:checked? @elite?
                                      #:enabled? @add?))])
           add-monster-states))
    (vpanel
      (hpanel set-picker monster-picker)
      (hpanel
        (apply vpanel (take add-monster-pickers 5))
        (apply vpanel (drop add-monster-pickers 5)))))

  )

(module+ main
  (require (submod ".." gui)
           racket/gui/easy)
  (define-values (info-db actions-db)
    (get-dbs "sample-db.rktd"))
  (void (render (window (single-monster-picker info-db)))))
