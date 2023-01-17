#lang racket

(provide
  #%app #%datum #%top #%top-interaction
  (rename-out [mb #%module-begin]))

(require syntax/parse/define
         racket/hash
         frosthaven-manager/monster-db
         (for-syntax syntax/parse
                     racket/list
                     racket/set
                     racket/string
                     racket/syntax
                     frosthaven-manager/defns
                     frosthaven-manager/qi
                     frosthaven-manager/monster-db))

;; e ::= '(import "path") | <monster-info> | listof <monster-ability>
(define-syntax-parse-rule (mb e:expr ...)
  #:with info-db (format-id this-syntax "info-db" #:source this-syntax)
  #:with ability-db (format-id this-syntax "ability-db" #:source this-syntax)
  #:with ((({~datum import} import) ...)
          (infos ...)
          ((actions ...) ...))
  (~> ((attribute e))
      sep
      (partition
        [(~> syntax->datum (and list? (~> first (equal? 'import)))) collect]
        [(~> syntax->datum monster-info?) collect]
        [(~> syntax->datum (and list? (andmap monster-ability? _))) collect]) collect)
  #:do [(define-values (imported-info-sets imported-ability-sets)
          (for/fold ([info-sets (set)]
                     [ability-sets (set)])
            ([import (in-list (syntax->datum #'(import ...)))])
            (define-values (imported-info-db imported-ability-db)
              (get-dbs import))
            (values (set-union info-sets (list->set (hash-keys imported-info-db)))
                    (set-union ability-sets (list->set (hash-keys imported-ability-db))))))
        (define sets
          (set-union
            imported-info-sets
            (list->set (map monster-info-set-name (syntax->datum #'(infos ...))))))
        (define ability-sets
          (set-union
            imported-ability-sets
            (list->set (map monster-ability-set-name (syntax->datum #'(actions ... ...))))))]
  #:fail-unless (subset? sets ability-sets)
  (format "these monster sets have no ability decks: ~a"
          (~> (sets ability-sets) set-subtract set->list (string-join ",")))
  #:with (imported-info-db ...) (generate-temporaries #'(import ...))
  #:with (imported-ability-db ...) (generate-temporaries #'(import ...))
  ;;=>
  (#%module-begin
   (provide info-db ability-db)
   (require (rename-in import
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
                   (error 'import-monsters "duplicate ability decks for set ~e" k))))))

(module reader syntax/module-reader
  frosthaven-manager/bestiary
  #:whole-body-readers? #t
  #:read-syntax read-syntax
  #:read read
  (require frosthaven-manager/parsers/monster)
  (define (read-syntax src in)
    (port-count-lines! in)
    (parse-bestiary src in #:syntax? #t))
  (define (read in)
    (port-count-lines! in)
    (parse-bestiary (object-name in) in #:syntax? #f)))
