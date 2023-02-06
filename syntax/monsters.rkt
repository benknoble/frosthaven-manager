#lang racket

(provide
  make-dbs
  (contract-out
    [syntaxes->bestiary-parts
      (-> (listof syntax?)
          (list/c (listof syntax?) (listof syntax?) (listof syntax?) (listof syntax?)))]
    [imports->dbs (-> (listof string?)
                      (values (listof info-db/c) (listof ability-db/c)))]
    [set-names-from-infos (-> (listof info-db/c) (listof monster-info?)
                              (set/c string?))]
    [ability-set-names-from-abilities (-> (listof ability-db/c) (listof monster-ability?)
                                          (set/c string?))]
    [monster-names-from-infos (-> (listof info-db/c) (listof monster-info?)
                                  (set/c string?))]
    [subset-error-message (-> string? string? set? set? string?)]))

(require syntax/parse/define
         racket/hash
         frosthaven-manager/qi
         frosthaven-manager/defns
         frosthaven-manager/monster-db
         frosthaven-manager/parsers/foes)

(define (syntaxish? p)
  (flow (~> syntax->datum p)))

(define-qi-syntax-parser collect-matching
  [(_ p-flo:expr ...) #'(partition [p-flo collect] ...)])

(define-qi-syntax-parser collect-matching/stx
  [(_ p-flo:expr ...) #'(collect-matching (syntaxish? p-flo) ...)])

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

(define-syntax-parse-rule (make-dbs ({~datum provide} info-db ability-db)
                                    ({~literal import} imports ...)
                                    ({~literal info} infos ...)
                                    ({~literal ability} (actions ...) ...))
  #:with (imported-info-db ...) (generate-temporaries #'(imports ...))
  #:with (imported-ability-db ...) (generate-temporaries #'(imports ...))
  (begin
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
