#lang racket

(provide
  #%app #%datum #%top #%top-interaction
  (rename-out [mb #%module-begin]))

(require syntax/parse/define
         frosthaven-manager/syntax/monsters
         (for-syntax racket/set
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
  ;;=>
  (#%module-begin
   (make-dbs (provide info-db ability-db)
             (import imports ...)
             (info infos ...)
             (ability (actions ...) ...))))

(module reader frosthaven-manager/syntax/module-reader
  frosthaven-manager/bestiary
  [parse-bestiary from frosthaven-manager/parsers/monster])
