#lang racket

;; TODO: GUI Integration

(provide
  #%app #%datum #%top #%top-interaction
  (rename-out [mb #%module-begin]))

(require syntax/parse/define
         racket/hash
         frosthaven-manager/defns
         frosthaven-manager/qi
         frosthaven-manager/monster-db
         (for-syntax syntax/parse
                     racket/list
                     racket/set
                     racket/string
                     racket/syntax
                     frosthaven-manager/defns
                     frosthaven-manager/qi
                     frosthaven-manager/monster-db
                     frosthaven-manager/parsers/foes))

;; e ::= '(import "path") | <monster-info> | listof <monster-ability> | <foe>
(define-syntax-parse-rule (mb e:expr ...)
  #:with info-db (format-id this-syntax "info-db" #:source this-syntax)
  #:with ability-db (format-id this-syntax "ability-db" #:source this-syntax)
  #:with make-foes (format-id this-syntax "make-foes" #:source this-syntax)
  #:with ((({~datum import} imports) ...)
          (infos ...)
          ((actions ...) ...)
          (foes ...))
  (~> ((attribute e))
      sep
      (partition
        [(~> syntax->datum (and list? (~> first (equal? 'import)))) collect]
        [(~> syntax->datum monster-info?) collect]
        [(~> syntax->datum (and list? (andmap monster-ability? _))) collect]
        [(~> syntax->datum foe/pc) collect]) collect)
  #:do [(define-values (imported-info-dbs imported-ability-dbs)
          (for/fold ([info-dbs empty]
                     [ability-dbs empty])
            ([import (in-list (syntax->datum #'(imports ...)))])
            (define-values (imported-info-db imported-ability-db)
              (get-dbs import))
            (values (cons imported-info-db info-dbs)
                    (cons imported-ability-db ability-dbs))))
        (define sets
          (apply set-union
                 (list->set (map monster-info-set-name (syntax->datum #'(infos ...))))
                 (map (flow (~> hash-keys list->set)) imported-info-dbs)))
        (define ability-sets
          (apply set-union
                 (list->set (map monster-ability-set-name (syntax->datum #'(actions ... ...))))
                 (map (flow (~> hash-keys list->set)) imported-ability-dbs)))
        (define monster-names
          (apply set-union
                 (list->set (map monster-info-name (syntax->datum #'(infos ...))))
                 (map (flow (~>> hash-values (append-map hash-keys) list->set)) imported-info-dbs)))
        (define foe-names
          (list->set (map second (syntax->datum #'(foes ...)))))]
  #:fail-unless (subset? sets ability-sets)
  (format "these monster sets have no ability decks: ~a"
          (~> (sets ability-sets) set-subtract set->list (string-join ",")))
  #:fail-unless (subset? foe-names monster-names)
  (format "these foes have no monster definitions: ~a"
          (~> (foe-names monster-names) set-subtract set->list (string-join ",")))
  #:with (imported-info-db ...) (generate-temporaries #'(imports ...))
  #:with (imported-ability-db ...) (generate-temporaries #'(imports ...))
  ;;=>
  (#%module-begin
   (provide info-db ability-db make-foes)
   (require (rename-in imports
                       [info-db imported-info-db]
                       [ability-db imported-ability-db]) ...)
   (define-values (original-info-db original-ability-db)
     (datums->dbs (list infos ... actions ... ...)))
   (define info-db
     (hash-union original-info-db imported-info-db ...
                 #:combine
                 (λ (ms1 ms2)
                   (hash-union ms1 ms2
                               #:combine/key
                               (λ (k _m1 _m2) (error 'import-monsters "duplicate definitions for monster ~e" k))))))
   (define ability-db
     (hash-union original-ability-db imported-ability-db ...
                 #:combine/key
                 (λ (k _as1 _as2)
                   (error 'import-monsters "duplicate ability decks for set ~e" k))))
   (define make-foes (make-foes-maker '(foes ...) info-db))))

(define ((make-foes-maker foes info-db) level number-of-players)
  (for/list ([foe (in-list foes)])
    (match-define (list set name numbering specs) foe)
    (define info (~> (info-db) (hash-ref set) (hash-ref name)))
    (define monster-types
      (~> (specs) sep
          (>< (hash-ref number-of-players))
          (pass (not (equal? "absent")))
          (>< (switch
                [(equal? "normal") #f]
                [(equal? "elite") #t]))
          collect))
    (define numbers
      (case numbering
        [(#f "ordered") (inclusive-range 1 (length monster-types))]
        [("random") (take (shuffle (inclusive-range 1 10)) (length monster-types))]))
    (make-monster-group info level (map cons numbers monster-types))))

(module reader syntax/module-reader
  frosthaven-manager/foes
  #:whole-body-readers? #t
  #:read-syntax read-syntax
  #:read read
  (require frosthaven-manager/parsers/foes)
  (define (read-syntax src in)
    (port-count-lines! in)
    (parse-foes src in #:syntax? #t))
  (define (read src in)
    (port-count-lines! in)
    (parse-foes src in #:syntax? #f)))

(module+ debug
  (provide view-foes)
  (require racket/gui/easy
           frosthaven-manager/observable-operator
           frosthaven-manager/gui/monsters)
  (define (view-foes foes)
    (render
      (window
        (apply vpanel
               (map (flow (~> @ simple-monster-group-view)) foes))))))
