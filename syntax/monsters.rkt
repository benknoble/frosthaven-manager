#lang racket
; vim: lw+=define/ability-sets,define/monster-names

(provide
  make-dbs
  (contract-out
    [syntaxes->bestiary-parts
      (-> (listof syntax?)
          (list/c (listof syntax?) (listof syntax?) (listof syntax?) (listof syntax?)))]
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
(require syntax/parse/define
         racket/hash
         racket/runtime-path
         frosthaven-manager/qi
         frosthaven-manager/defns
         frosthaven-manager/monster-db
         frosthaven-manager/parsers/foes)

(define-syntax-parse-rule (define/ability-sets (f sets ability-sets) body ...+)
  (define (f imported-info-dbs imported-ability-dbs infos actions)
    (define sets (set-names-from-infos imported-info-dbs infos))
    (define ability-sets (ability-set-names-from-abilities imported-ability-dbs actions))
    body ...))

(define-syntax-parse-rule (define/monster-names (f monster-names foe-names) body ...+)
  (define (f imported-info-dbs infos foes)
    (define monster-names (monster-names-from-infos imported-info-dbs infos))
    (define foe-names (list->set (map second foes)))
    body ...))

;;;; exports
(define-syntax-parse-rule (make-dbs ({~literal provide} info-db ability-db)
                                    ({~datum import} imports ...)
                                    ({~datum info} infos ...)
                                    ({~datum ability} (actions ...) ...))
  #:with (imported-info-db ...) (generate-temporaries #'(imports ...))
  #:with (imported-ability-db ...) (generate-temporaries #'(imports ...))
  ;; also binds `here` correctly
  #:with runtime-path-define (datum->syntax #'info-db (syntax-e #'(define-runtime-path here ".")) #'info-db)
  (begin
    (provide info-db ability-db)
    (require (rename-in imports
                        [info-db imported-info-db]
                        [ability-db imported-ability-db]) ...)
    runtime-path-define
    (define-values (original-info-db original-ability-db)
      (datums->dbs (list infos ... (struct-copy monster-ability actions [location here]) ... ...)))
    (define info-db
      (combine-infos original-info-db imported-info-db ...))
    (define ability-db
      (combine-abilities original-ability-db imported-ability-db ...))))

;; -> imports monster-infos ability-decks foes
(define-flow (syntaxes->bestiary-parts syntaxes)
  (~> sep (collect-matching/stx (list/c 'import string?)
                                monster-info?
                                (listof monster-ability?)
                                foe/pc) collect))

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
(define (syntaxish? p)
  (flow (~> syntax->datum p)))

(define-qi-syntax-parser collect-matching
  [(_ p-flo:expr ...) #'(partition [p-flo collect] ...)])

(define-qi-syntax-parser collect-matching/stx
  [(_ p-flo:expr ...) #'(collect-matching (syntaxish? p-flo) ...)])

(define-flow hash-keys/set (~> hash-keys list->set))

(define ((make-set-names get-set-name) dbs xs)
  (apply set-union
         (~>> (xs) (map get-set-name) list->set)
         (map hash-keys/set dbs)))

(define set-names-from-infos (make-set-names monster-info-set-name))
(define ability-set-names-from-abilities (make-set-names monster-ability-set-name))
(define (monster-names-from-infos info-dbs infos)
  (apply set-union
         (~>> (infos) (map monster-info-name) list->set)
         (map (flow (~>> hash-values (append-map hash-keys) list->set)) info-dbs)))

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
