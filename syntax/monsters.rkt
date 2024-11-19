#lang racket
; vim: lw+=define/ability-sets,define/monster-names,match-loop

(provide
  make-dbs
  (contract-out
    [imports->dbs (-> (listof string?)
                      (values (listof info-db/c) (listof ability-db/c)))]
    [check-monsters-have-abilities (-> (listof info-db/c) (listof ability-db/c)
                                       (listof monster-info?) (listof monster-ability?)
                                       boolean?)]
    [check-monsters-have-abilities-message (-> (listof info-db/c) (listof ability-db/c)
                                               (listof monster-info?) (listof monster-ability?)
                                               string?)]
    [check-foes-have-monsters (-> (listof info-db/c) (listof monster-info?) (listof foe/pc)
                                  boolean?)]
    [check-foes-have-monsters-message (-> (listof info-db/c) (listof monster-info?) (listof foe/pc)
                                          string?)]))

;;;; requires and implementation macros
(require (for-syntax frosthaven-manager/defns/monsters
                     frosthaven-manager/rich-text-helpers
                     racket
                     racket/syntax)
         frosthaven-manager/curlique
         frosthaven-manager/defns
         frosthaven-manager/monster-db
         frosthaven-manager/parsers/foes
         racket/hash
         syntax/parse/define)

(define-syntax-parser define/ability-sets
  [(_ (f sets ability-sets) body ...+)
   (syntax/loc this-syntax
     (define (f imported-info-dbs imported-ability-dbs infos actions)
       (define sets (set-names-from-infos imported-info-dbs infos))
       (define ability-sets (ability-set-names-from-abilities imported-ability-dbs actions))
       body ...))])

(define-syntax-parser define/monster-names
  [(_ (f monster-names foe-names) body ...+)
   (syntax/loc this-syntax
     (define (f imported-info-dbs infos foes)
       (define monster-names (monster-names-from-infos imported-info-dbs infos))
       (define foe-names (list->set (map second foes)))
       body ...))])

;;;; exports
(define-syntax-parser make-dbs
  [(_ ({~literal provide} info-db ability-db)
      ({~datum import} imports ...)
      ({~datum info} infos ...)
      ({~datum ability} actions ...))
   #:with (imported-info-db ...) (generate-temporaries #'(imports ...))
   #:with (imported-ability-db ...) (generate-temporaries #'(imports ...))
   (syntax/loc this-syntax
     (begin
       (provide info-db ability-db)
       (require (rename-in imports
                           [info-db imported-info-db]
                           [ability-db imported-ability-db]) ...)
       (define-values (original-info-db original-ability-db)
         (datums->dbs (list infos ... (process-aoes actions) ...)))
       (define info-db
         (combine-infos original-info-db imported-info-db ...))
       (define ability-db
         (combine-abilities original-ability-db imported-ability-db ...))))])

(define (imports->dbs imports)
  (for/fold ([info-dbs empty]
             [ability-dbs empty])
    ([import (in-list imports)])
    (~> (import) get-dbs (== (cons info-dbs) (cons ability-dbs)))))

(define/ability-sets (check-monsters-have-abilities sets ability-sets)
  (subset? sets ability-sets))

(define/ability-sets (check-monsters-have-abilities-message sets ability-sets)
  (subset-error-message "monster sets" "ability decks" sets ability-sets))

(define/monster-names (check-foes-have-monsters monster-names foe-names)
  (subset? foe-names monster-names))

(define/monster-names (check-foes-have-monsters-message monster-names foe-names)
  (subset-error-message "foes" "monster definition" foe-names monster-names))

;;;; helper definitions
(define-syntax-parser process-aoes
  [(_ action:expr)
   (define ability-card (syntax->datum #'action))
   (match-define (monster-ability set-name name initiative abilities shuffle?)
     ability-card)
   (with-syntax ([(parts ...) (map (splice-aoes (attribute action)) abilities)])
     (quasisyntax/loc this-syntax
       (monster-ability
        #,set-name
        #,name
        #,initiative
        ;; correctness: see splice-aoes' use of #'list. The computed list of syntax
        ;; objects each represent a value computation, but the list is not an
        ;; application. Use (list expr …) to represent a list computation.
        (list parts ...)
        #,shuffle?)))])

;; ability: listof (or/c string? pict?)
(define-for-syntax ((splice-aoes original-syntax) ability)
  (define (make-original datum)
    (datum->syntax original-syntax datum
                   original-syntax original-syntax))
  (define aoe->id-map
    (find-all-aoes ability))
  (define with-substitutions
    (append-map (only-on-text substitute-aoe aoe->id-map)
                ability))
  (define new-expression
    ;; hygiene: use list from this module!
    ;; correctness: parts is a list of expressions, but we want the result to be
    ;; computed as a list, not as an application. It's not implicitly quoted in
    ;; the desired result (unlike when it was the input to process-aoes).
    (cons #'list with-substitutions))
  (define expr-stx (make-original new-expression))
  (for/fold ([res expr-stx])
            ([(aoe-path id) (in-hash aoe->id-map)])
    (syntax-local-lift-require
     #`(rename (file #,aoe-path) #,id aoe)
     res)))

(define-for-syntax (find-all-aoes ability)
  (for/hash ([part (in-list ability)]
             #:when (string? part)
             [aoe-spec (in-list (regexp-match* aoe-rx part #:match-select second))])
    ;; Here be dragons: messing with scopes on the generated-temporary is likely
    ;; to break things. Example: using `syntax-e` to compute entirely with
    ;; non-syntax data means the id use in substitute-aoe (after syntaxifying
    ;; it) and the id use in syntax-local-lift-require from splice-aoes get
    ;; different module scopes and don't bind each other. Blech!
    (values aoe-spec (generate-temporary aoe-spec))))

(define-for-syntax (substitute-aoe aoe->id-map part)
  (match-loop part
    [(regexp aoe-rx (list _ prefix aoe suffix))
     ;; Compute an expr like (make-aoe)
     (define aoe-pict-expr
       (list (hash-ref aoe->id-map aoe)))
     (list prefix aoe-pict-expr suffix)]))

(define-for-syntax aoe-rx #rx"aoe\\(([^)]+)\\)")

(define hash-keys/set {~> hash-keys list->set})

(define ((make-set-names get-set-name) dbs xs)
  (apply set-union
         (~>> (xs) (map get-set-name) list->set)
         (map hash-keys/set dbs)))

(define set-names-from-infos (make-set-names monster-info-set-name))
(define ability-set-names-from-abilities (make-set-names monster-ability-set-name))
(define (monster-names-from-infos info-dbs infos)
  (apply set-union
         (~>> (infos) (map monster-info-name) list->set)
         (map {~>> hash-values (append-map hash-keys) list->set} info-dbs)))

(define (subset-error-message who what should-be-smaller should-be-larger)
  (format "these ~a have no ~a: ~a"
          who what
          (~> (should-be-smaller should-be-larger) set-subtract set->list (string-join ","))))

(define (combine-infos base . extra)
  (apply hash-union base extra
         #:combine
         (λ (ms1 ms2)
           (hash-union ms1 ms2
                       #:combine/key
                       (λ (k _m1 _m2) (error 'import-monsters "duplicate definitions for monster ~e" k))))))

(define (combine-abilities base . extra)
  (apply hash-union base extra
         #:combine/key
         (λ (k _as1 _as2)
           (error 'import-monsters "duplicate ability decks for set ~e" k))))
