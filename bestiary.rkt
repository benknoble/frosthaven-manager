#lang racket

(provide
  #%app #%datum #%top
  (rename-out [mb #%module-begin]))

(require syntax/parse/define
         frosthaven-manager/monster-db
         (for-syntax syntax/parse
                     racket/list
                     racket/set
                     racket/string
                     frosthaven-manager/defns
                     frosthaven-manager/qi))

;; e ::= <monster-info> | listof <monster-ability>
(define-syntax-parse-rule (mb e:expr ...)
  #:with info-db (datum->syntax #f 'info-db)
  #:with action-db (datum->syntax #f 'action-db)
  #:with ((infos ...) ((actions ...) ...))
  (let-values ([(infos actions) (partition (flow (~> syntax-e monster-info?)) (attribute e))])
    (list infos actions))
  #:do [(define sets (list->set (map monster-info-set-name (syntax->datum #'(infos ...)))))
        (define ability-sets (list->set (map monster-ability-set-name (syntax->datum #'(actions ... ...)))))]
  #:fail-unless (subset? sets ability-sets)
  (format "these monster sets have no ability decks: ~a"
          (~> (sets ability-sets) set-subtract set->list (string-join ",")))
  ;;=>
  (#%module-begin
   (provide info-db action-db)
   (define-values (info-db action-db)
     (datums->dbs (list infos ... actions ... ...)))))

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
