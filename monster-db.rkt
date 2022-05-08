#lang racket

(provide (contract-out
           [info-db/c contract?]
           [action-db/c contract?]
           [get-dbs (-> path-string? (values info-db/c action-db/c))]
           [default-monster-db path-string?]))

(require racket/runtime-path
         "qi.rkt"
         "defns.rkt")

(define info-db/c
  (hash/c string? (hash/c string? monster-info?)))
(define action-db/c
  (hash/c string? (listof monster-action?)))

(define-runtime-path default-monster-db "monster-db.rktd")

(define (get-dbs db-file)
  (~> (db-file)
      file->list sep
      (partition
        ;; info db
        [monster-info? (~>> collect (group-by monster-info-set-name)
                            (list~>hash #:->key (~> car monster-info-set-name)
                                        #:->value (list~>hash #:->key monster-info-name)))]
        ;; actions deck
        [monster-action? (~>> collect (group-by monster-action-set-name)
                              (list~>hash #:->key (~> car monster-action-set-name)))])))

(define-flow take-first (~> hash-keys car))
(define (initial-set+info info-db)
  (define set (take-first info-db))
  (define name->info (hash-ref info-db set))
  (define info (hash-ref name->info (take-first name->info)))
  (values set info))

(module+ gui
  (provide (contract-out
             [single-monster-event/c contract?]
             [single-monster-picker (->* (info-db/c)
                                         (#:on-change (-> single-monster-event/c any))
                                         (is-a?/c view<%>))]))

  (require racket/gui/easy
           "observable-operator.rkt")

  (define single-monster-event/c
    (or/c
      (list/c 'set 'from string? 'to string?)
      (list/c 'monster 'from monster-info? 'to monster-info?)
      (list/c 'include? (integer-in 1 10) 'to boolean?)
      (list/c 'elite? (integer-in 1 10) 'to boolean?)))

  ;; TODO: monster-view
  ;; some similarities to player-view, but need room for actions and overall
  ;; stats, the monster numbers + elite? status, ability to "hide" action, etc.

  (define (single-monster-picker info-db #:on-change [on-change void])
    (define sets (hash-keys info-db))
    (define-values (set info) (initial-set+info info-db))
    (define/obs @set set)
    (define/obs @info info)
    (define @name->info (@~> @set (hash-ref info-db _)))
    (define (choose-set set)
      (on-change `(set from ,(@! @set) to ,set))
      (:= @set set))
    (define set-picker
      (choice #:label "Set" sets choose-set))
    (define @valid-monsters (@> @name->info hash-keys))
    (define (choose-monster monster-name)
      (define new-info (hash-ref (@! @name->info) monster-name))
      (on-change `(monster from ,(@! @info) to ,new-info))
      (:= @info new-info))
    (define monster-picker
      (choice #:label "Monster" @valid-monsters choose-monster))
    (define (make-monster-selector num)
      (define/obs @included? #f)
      (define (set-included included?)
        (on-change `(include? ,num to ,included?))
        (:= @included? included?))
      (define (set-elite elite?)
        (on-change `(elite? ,num to ,elite?)))
      (hpanel (checkbox set-included #:label (~a num))
              (checkbox set-elite #:label "Elite?" #:enabled? @included?)))
    (vpanel (hpanel set-picker monster-picker
                    #:alignment '(center top)
                    #:stretch '(#f #f))
            (hpanel (apply vpanel (map make-monster-selector (inclusive-range 1 5)))
                    (apply vpanel (map make-monster-selector (inclusive-range 6 10))))))

  )

(module+ main
  (require (submod ".." gui)
           racket/gui/easy
           "observable-operator.rkt")
  (define-values (info-db actions-db)
    (get-dbs "sample-db.rktd"))
  (define-values (set info) (initial-set+info info-db))
  (define/obs @state
    ;; 0: set
    ;; 1: info
    ;; 2: hash number -> elite
    (list set info (hash)))
  (void
    (render
      (window
        (single-monster-picker
          info-db
          #:on-change
          (match-lambda
            [`(set from ,old to ,new) (<~@ @state (list-set 0 new))]
            [`(monster from ,old to ,new) (<~@ @state (list-set 1 new))]
            [`(include? ,n to #t)
              (<~@ @state (list-update 2 (flow (hash-update n values #f))))]
            [`(include? ,n to #f)
              (<~@ @state (list-update 2 (flow (hash-remove n))))]
            [`(elite? ,n to ,elite?)
              ;; looks like hash-set, but I want the missing-key semantics of
              ;; hash-update with no failure-result as a guard against bugs
              (<~@ @state (list-update 2 (flow (hash-update n (const elite?)))))])))))
  (void
    (render
      (window
        (vpanel #:min-size '(100 300)
                (text (@~> @state (~> (-< car " " (~> cadr (if _ monster-info-name ~a))) ~a)))
                (list-view (@~> @state (~> caddr (hash->list #t))) #:key car
                  (λ (k @e)
                    (text (@~> @e (~>> cdr (if _ "Elite" "Normal") (~a k ": ")))))))))))

(module+ test
  (require rackunit)
  (define-values (info actions)
    (get-dbs "sample-db.rktd"))
  (check-equal? info
                #hash(("archer"
                       .
                       #hash(("hynox archer"
                              .
                              #s(monster-info
                                  "archer"
                                  "hynox archer"
                                  (#s(monster-stats 2 2 2 () () ())
                                   #s(monster-stats 3 3 3 () () ())
                                   #s(monster-stats 4 4 4 () () ())
                                   #s(monster-stats 5 5 5 () () ())
                                   #s(monster-stats 6 6 6 () () ())
                                   #s(monster-stats 7 7 7 () () ())
                                   #s(monster-stats 8 8 8 () () ())
                                   #s(monster-stats 9 9 9 () () ()))
                                  (#s(monster-stats 4 2 3 ("shield 1") () ())
                                   #s(monster-stats 5 3 4 ("shield 1") () ())
                                   #s(monster-stats 6 4 5 ("shield 1") () ())
                                   #s(monster-stats 7 5 6 ("shield 2") () ())
                                   #s(monster-stats 8 6 7 ("shield 2") () ())
                                   #s(monster-stats 9 7 8 ("shield 2") () ())
                                   #s(monster-stats 10 8 9 ("shield 3") () ())
                                   #s(monster-stats 11 9 10 ("shield 3") () ()))))
                             ("wyrmling archer"
                              .
                              #s(monster-info
                                  "archer"
                                  "wyrmling archer"
                                  (#s(monster-stats 1 1 1 () () ())
                                   #s(monster-stats 2 2 2 () () ())
                                   #s(monster-stats 3 3 3 () () ())
                                   #s(monster-stats 4 4 4 () () ())
                                   #s(monster-stats 5 5 5 () () ())
                                   #s(monster-stats 6 6 6 () () ())
                                   #s(monster-stats 7 7 7 () () ())
                                   #s(monster-stats 8 8 8 () () ()))
                                  (#s(monster-stats 3 1 2 ("shield 1") () ())
                                   #s(monster-stats 4 2 3 ("shield 1") () ())
                                   #s(monster-stats 5 3 4 ("shield 1") () ())
                                   #s(monster-stats 6 4 5 ("shield 2") () ())
                                   #s(monster-stats 7 5 6 ("shield 2") () ())
                                   #s(monster-stats 8 6 7 ("shield 2") () ())
                                   #s(monster-stats 9 7 8 ("shield 3") () ())
                                   #s(monster-stats 10 8 9 ("shield 3") () ()))))))
                      ("guard"
                       .
                       #hash(("hynox guard"
                              .
                              #s(monster-info
                                  "guard"
                                  "hynox guard"
                                  (#s(monster-stats 2 2 2 () () ())
                                   #s(monster-stats 3 3 3 () () ())
                                   #s(monster-stats 4 4 4 () () ())
                                   #s(monster-stats 5 5 5 () () ())
                                   #s(monster-stats 6 6 6 () () ())
                                   #s(monster-stats 7 7 7 () () ())
                                   #s(monster-stats 8 8 8 () () ())
                                   #s(monster-stats 9 9 9 () () ()))
                                  (#s(monster-stats 4 2 3 ("shield 1") () ())
                                   #s(monster-stats 5 3 4 ("shield 1") () ())
                                   #s(monster-stats 6 4 5 ("shield 1") () ())
                                   #s(monster-stats 7 5 6 ("shield 2") () ())
                                   #s(monster-stats 8 6 7 ("shield 2") () ())
                                   #s(monster-stats 9 7 8 ("shield 2") () ())
                                   #s(monster-stats 10 8 9 ("shield 3") () ())
                                   #s(monster-stats 11 9 10 ("shield 3") () ()))))))))
  (check-equal? actions
                #hash(("archer"
                       .
                       (#s(monster-action
                            "archer"
                            "double-shot"
                            25
                            ("move +1"
                             "attack +2, range 5"
                             "attack +2, range 5, +1 if same target")
                            #f)
                        #s(monster-action
                            "archer"
                            "take aim"
                            80
                            ("move +2" "strengthen self")
                            #t)))
                      ("guard"
                       .
                       (#s(monster-action
                            "guard"
                            "rushing charge"
                            25
                            ("move +3" "attack +2 + number of spaces moved towards target")
                            #f)
                        #s(monster-action "guard" "stand tall" 80 ("shield 3") #t))))))
