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
             [single-monster-picker (-> info-db/c
                                        (values (obs/c (listof (list/c (integer-in 1 10)
                                                                       boolean?
                                                                       boolean?)))
                                                (obs/c monster-info?)
                                                (is-a?/c view<%>)))]))

  (require racket/gui/easy
           racket/gui/easy/operator
           racket/gui/easy/contract)

  ;; TODO: monster-view
  ;; some similarities to player-view, but need room for actions and overall
  ;; stats, the monster numbers + elite? status, ability to "hide" action, etc.

  ;; TODO: how to give back state of which monsters were added?
  ;; can we just return the add-monster-states, too?
  (define (single-monster-picker info-db #|@monsters|#)
    (define sets (hash-keys info-db))
    (define/obs @set (car sets))
    (define @set-map (~> @set (flow (hash-ref info-db _))))
    (define set-picker (choice #:label "Set" sets (λ:= @set identity)))
    (define @valid-monsters (~> @set-map hash-keys))
    (define/obs @info (on (@valid-monsters)
                        (~>> obs-peek
                             car
                             (hash-ref (obs-peek @set-map)))))
    (define monster-picker (choice #:label "Monster" @valid-monsters
                                   (λ:= @info (flow (hash-ref (obs-peek @set-map) _)))))
    (define @add-monster-states
      (@ (for/list ([num (in-inclusive-range 1 10)])
           (list num #|elite?|# #f #|to-add?|# #f))))
    (define ((update-@add-monster-states k proc) add-monster-states)
      (for/list ([e (in-list add-monster-states)])
        (match e
          [(list (== k) _ _) (proc e)]
          [_ e])))
    (define (make-monster-picker k @e)
      (hpanel
        (checkbox (λ (added?)
                    (<~ @add-monster-states
                        (update-@add-monster-states
                          k
                          (match-lambda
                            [(list (== k) elite? _)
                             (list k elite? added?)]))))
                  #:label (~> @e (flow (~> car ~a)))
                  #:checked? (~> @e caddr))
        (checkbox (λ (elite?)
                    (<~ @add-monster-states
                        (update-@add-monster-states
                          k
                          (match-lambda
                            [(list (== k) _ added?)
                             (list k elite? added?)]))))
                  #:label "Elite?"
                  #:checked? (~> @e cadr)
                  #:enabled? (~> @e caddr))))
    (define picker
      (vpanel
        (hpanel set-picker monster-picker
                #:alignment '(center top)
                #:stretch '(#f #f))
        (hpanel (list-view (~> @add-monster-states (flow (take 5)))
                           make-monster-picker
                           #:key car
                           #:stretch '(#t #f)
                           #:min-size '(#f 120))
                (list-view (~> @add-monster-states (flow (drop 5)))
                           make-monster-picker
                           #:key car
                           #:stretch '(#t #f)
                           #:min-size '(#f 120)))))
    (values @add-monster-states @info picker))

  )

(module+ main
  (require (submod ".." gui)
           racket/gui/easy)
  (define-values (info-db actions-db)
    (get-dbs "sample-db.rktd"))
  (define-values (@monster-states @info picker)
    (single-monster-picker info-db))
  (void (render (window picker)))
  (void (render (window
                  (vpanel
                    (text (obs-map @info monster-info-name))
                    (list-view @monster-states
                               (λ (k @e)
                                 (text (obs-map @e ~a)))
                               #:key car))))))
