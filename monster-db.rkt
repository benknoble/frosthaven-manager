#lang racket

(provide
  (contract-out
    [info-db/c contract?]
    [ability-db/c contract?]
    [datums->dbs (-> (listof any/c) (values info-db/c ability-db/c))]
    [get-dbs (-> path-string? (values info-db/c ability-db/c))]
    [default-monster-db path-string?]))

(require frosthaven-manager/curlique
         frosthaven-manager/defns
         frosthaven-manager/qi/list2hash
         racket/runtime-path)

(define info-db/c
  ;; Monster Set -> Monster Name -> Monster Info
  (hash/c string? (hash/c string? monster-info?)))
(define ability-db/c
  ;; Monster Set -> Monster Deck
  (hash/c string? (listof monster-ability?)))

(define-runtime-path default-monster-db "sample-bestiary.rkt")

(define (get-dbs db-file)
  (values (dynamic-require db-file 'info-db)
          (dynamic-require db-file 'ability-db)))

(define datums->dbs
  {~> sep
      (partition
       ;; info db
       [monster-info? (~>> collect (group-by monster-info-set-name)
                           (list~>hash #:->key (~> car monster-info-set-name)
                                       #:->value (list~>hash #:->key monster-info-name)))]
       ;; ability deck
       [monster-ability? (~>> collect (group-by monster-ability-set-name)
                              (list~>hash #:->key (~> car monster-ability-set-name)))])})

(module+ test
  (require rackunit)
  (define sample-info
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
  (define sample-abilities
    #hash(("archer"
           .
           (#s(monster-ability "archer" "double-shot" 25 (("attack +2, range 5") ("attack +2, range 3, +1 if same target")) #f)
            #s(monster-ability "archer" "double-shot" 25 (("attack +2, range 5") ("attack +2, range 3, +1 if same target")) #f)
            #s(monster-ability "archer" "double-shot" 25 (("attack +2, range 5") ("attack +2, range 3, +1 if same target")) #f)
            #s(monster-ability "archer" "double-shot" 25 (("attack +2, range 5") ("attack +2, range 3, +1 if same target")) #f)
            #s(monster-ability "archer" "double-shot" 25 (("attack +2, range 5") ("attack +2, range 3, +1 if same target")) #f)
            #s(monster-ability "archer" "double-shot" 25 (("attack +2, range 5") ("attack +2, range 3, +1 if same target")) #f)
            #s(monster-ability "archer" "double-shot" 25 (("attack +2, range 5") ("attack +2, range 3, +1 if same target")) #f)
            #s(monster-ability "archer" "take aim" 80 (("move +2") ("strengthen self")) #t)))
          ("guard"
           .
           (#s(monster-ability "guard" "rushing charge" 25 (("move +3") ("attack +2 + number of spaces moved towards target")) #f)
            #s(monster-ability "guard" "rushing charge" 25 (("move +3") ("attack +2 + number of spaces moved towards target")) #f)
            #s(monster-ability "guard" "rushing charge" 25 (("move +3") ("attack +2 + number of spaces moved towards target")) #f)
            #s(monster-ability "guard" "rushing charge" 25 (("move +3") ("attack +2 + number of spaces moved towards target")) #f)
            #s(monster-ability "guard" "rushing charge" 25 (("move +3") ("attack +2 + number of spaces moved towards target")) #f)
            #s(monster-ability "guard" "rushing charge" 25 (("move +3") ("attack +2 + number of spaces moved towards target")) #f)
            #s(monster-ability "guard" "rushing charge" 25 (("move +3") ("attack +2 + number of spaces moved towards target")) #f)
            #s(monster-ability "guard" "stand tall" 80 (("shield 3")) #t)))))
  (let-values ([(info abilities) (get-dbs default-monster-db)])
    (check-equal? info sample-info)
    (check-equal? abilities sample-abilities))
  ;; only check that it loads
  (match-let-values ([(_ _) (get-dbs "testfiles/sample-bestiary-import.rkt")])
    (void)))
