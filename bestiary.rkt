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
  #:attr sets (list->set (map monster-info-set-name (syntax->datum #'(infos ...))))
  #:attr ability-sets (list->set (map monster-ability-set-name (syntax->datum #'(actions ... ...))))
  #:fail-unless (subset? (attribute sets) (attribute ability-sets))
  (format "these monster sets have no ability decks: ~a"
          (~> ((attribute sets) (attribute ability-sets))
              set-subtract set->list (string-join ",")))
  ;;=>
  (#%module-begin
   (provide info-db action-db)
   (define-values (info-db action-db)
     (datums->dbs (list infos ... actions ... ...)))))

(module reader racket
  (provide read-syntax)
  (require syntax/strip-context
           frosthaven-manager/parsers/monster)
  (define (read-syntax src in)
    (port-count-lines! in)
    (define bestiary (parse-bestiary src in #:syntax? #t))
    (strip-context
      #`(module bestiary frosthaven-manager/bestiary
          #,@bestiary))))
