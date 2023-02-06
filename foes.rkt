#lang racket

(provide
  #%app #%datum #%top #%top-interaction
  (rename-out [mb #%module-begin]))

(require syntax/parse/define
         racket/hash
         frosthaven-manager/defns
         frosthaven-manager/qi
         frosthaven-manager/syntax/monsters
         (for-syntax racket/set
                     racket/list
                     racket/syntax
                     frosthaven-manager/syntax/monsters))

;; e ::= '(import "path") | <monster-info> | listof <monster-ability> | <foe>
(define-syntax-parse-rule (mb e:expr ...)
  #:with info-db (format-id this-syntax "info-db" #:source this-syntax)
  #:with ability-db (format-id this-syntax "ability-db" #:source this-syntax)
  #:with make-foes (format-id this-syntax "make-foes" #:source this-syntax)
  #:with ((({~datum import} imports) ...)
          (infos ...)
          ((actions ...) ...)
          (foes ...))
  (syntaxes->bestiary-parts (attribute e))
  #:do [(define-values (imported-info-dbs imported-ability-dbs)
          (imports->dbs (syntax->datum #'(imports ...))))
        (define monster-names
          (monster-names-from-infos imported-info-dbs (syntax->datum #'(infos ...))))
        (define foe-names
          (list->set (map second (syntax->datum #'(foes ...)))))]
  #:fail-unless (check-monsters-have-abilities imported-info-dbs imported-ability-dbs
                                               (syntax->datum #'(infos ...))
                                               (syntax->datum #'(actions ... ...)))
  (check-monsters-have-abilities-message imported-info-dbs imported-ability-dbs
                                         (syntax->datum #'(infos ...))
                                         (syntax->datum #'(actions ... ...)))
  #:fail-unless (subset? foe-names monster-names)
  (subset-error-message "foes" "monster definition" foe-names monster-names)
  ;;=>
  (#%module-begin
   (make-dbs (provide info-db ability-db)
             (import imports ...)
             (info infos ...)
             (ability (actions ...) ...))
   (provide make-foes)
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

(module reader frosthaven-manager/syntax/module-reader
  frosthaven-manager/foes
  [parse-foes from frosthaven-manager/parsers/foes])

(module+ debug
  (provide view-foes
           view-all-foes)
  (require racket/gui/easy
           frosthaven-manager/observable-operator
           frosthaven-manager/gui/monsters)
  (define (view-all-foes make-foes level)
    (for-each (compose1 view-foes make-foes)
              (list level level level)
              '(2 3 4)))
  (define (view-foes foes)
    (render
      (window
        (apply vpanel
               (map (flow (~> @ simple-monster-group-view)) foes))))))

(module+ test
  (require racket/runtime-path rackunit)
  (define-runtime-path test-foes.rkt "testfiles/sample-foes.rkt")
  (define make-foes (dynamic-require test-foes.rkt 'make-foes))
  ;; > (random-seed 0)
  ;; > (for/list ([_ 3])
  ;;     (first (shuffle (inclusive-range 1 10))))
  ;; '(3 10 10)
  (random-seed 0)
  (check-equal? (make-foes 4 2)
                (list
                  (monster-group "archer" "wyrmling archer" 4
                                 #s(monster-stats 5 5 5 () () ())
                                 #s(monster-stats 7 5 6 ("shield 2") () ())
                                 (list (monster 1 #f 5 empty)))
                  (monster-group "guard" "hynox guard" 4
                                 #s(monster-stats 6 6 6 () () ())
                                 #s(monster-stats 8 6 7 ("shield 2") () ())
                                 (list (monster 3 #t 8 empty)))))
  (check-equal? (make-foes 5 3)
                (list
                  (monster-group "archer" "wyrmling archer" 5
                                 #s(monster-stats 6 6 6 () () ())
                                 #s(monster-stats 8 6 7 ("shield 2") () ())
                                 (list
                                   (monster 2 #t 8 empty)
                                   (monster 1 #f 6 empty)))
                  (monster-group "guard" "hynox guard" 5
                                 #s(monster-stats 7 7 7 () () ())
                                 #s(monster-stats 9 7 8 ("shield 2") () ())
                                 (list (monster 10 #t 9 empty)))))
  (check-equal? (make-foes 6 4)
                (list
                  (monster-group "archer" "wyrmling archer" 6
                                 #s(monster-stats 7 7 7 () () ())
                                 #s(monster-stats 9 7 8 ("shield 3") () ())
                                 (list
                                   (monster 1 #t 9 empty)
                                   (monster 2 #t 9 empty)))
                  (monster-group "guard" "hynox guard" 6
                                 #s(monster-stats 8 8 8 () () ())
                                 #s(monster-stats 10 8 9 ("shield 3") () ())
                                 (list (monster 10 #t 10 empty))))))
