#lang racket

(provide
  #%app #%datum #%top #%top-interaction
  (rename-out [mb #%module-begin]))

(require (for-syntax frosthaven-manager/syntax/monsters)
         frosthaven-manager/syntax/monsters
         syntax/parse/define)

(define-syntax-parser mb
  [(_ ({~datum import} imports:string ...)
      ({~datum info} infos ...)
      ({~datum ability} actions ...))
   #:do [(define-values (imported-info-dbs imported-ability-dbs)
           (imports->dbs (syntax->datum #'(imports ...))))]
   #:fail-unless (check-monsters-have-abilities imported-info-dbs imported-ability-dbs
                                                (syntax->datum #'(infos ...))
                                                (syntax->datum #'(actions ...)))
   (check-monsters-have-abilities-message imported-info-dbs imported-ability-dbs
                                          (syntax->datum #'(infos ...))
                                          (syntax->datum #'(actions ...)))
   ;;=>
   (syntax/loc this-syntax
     (#%module-begin
      (make-dbs (provide info-db ability-db)
                (import imports ...)
                (info infos ...)
                (ability actions ...))))])

(module reader frosthaven-manager/syntax/module-reader
  frosthaven-manager/bestiary
  [parse-bestiary from frosthaven-manager/parsers/monster])
