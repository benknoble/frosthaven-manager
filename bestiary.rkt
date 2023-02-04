#lang racket

(provide
  #%app #%datum #%top #%top-interaction
  (rename-out [mb #%module-begin]))

(require syntax/parse/define
         racket/hash
         frosthaven-manager/monster-db
         (for-syntax syntax/parse
                     racket/set
                     racket/syntax
                     frosthaven-manager/syntax/monsters))

;; e ::= '(import "path") | <monster-info> | listof <monster-ability>
(define-syntax-parse-rule (mb e:expr ...)
  #:with info-db (format-id this-syntax "info-db" #:source this-syntax)
  #:with ability-db (format-id this-syntax "ability-db" #:source this-syntax)
  #:with ((({~datum import} imports) ...)
          (infos ...)
          ((actions ...) ...)
          _)
  (syntaxes->bestiary-parts (attribute e))
  #:do [(define-values (imported-info-dbs imported-ability-dbs)
          (imports->dbs (syntax->datum #'(imports ...))))
        (define sets
          (set-names-from-infos imported-info-dbs (syntax->datum #'(infos ...))))
        (define ability-sets
          (ability-set-names-from-abilities imported-ability-dbs (syntax->datum #'(actions ... ...))))]
  #:fail-unless (subset? sets ability-sets)
  (subset-error-message "monster sets" "ability decks" sets ability-sets)
  #:with (imported-info-db ...) (generate-temporaries #'(imports ...))
  #:with (imported-ability-db ...) (generate-temporaries #'(imports ...))
  ;;=>
  (#%module-begin
   (provide info-db ability-db)
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
