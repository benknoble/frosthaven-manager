#lang racket

(provide
 (enum-out element)
 (enum-out monster-modifier)
 (enum-out condition)
 (contract-out
  [format-element (-> element? string?)]
  [parse-element (-> string? element?)]
  [format-monster-modifier (-> monster-modifier? string?)]
  [parse-monster-modifier (-> string? monster-modifier?)]
  [format-condition (-> condition? string?)]
  [parse-condition (-> string? condition?)]
  [initiative? predicate/c]
  [ability? predicate/c]
  [conditions (listof condition?)]
  [discriminator:condition (-> condition? integer?)]
  [selector:condition (-> integer? condition?)]
  [expirable-conditions (set/c condition?)]
  [monster-modifier-deck (listof monster-modifier?)]
  [shuffle-modifier-deck? (-> (listof monster-modifier?) boolean?)]
  [better-modifier (-> monster-modifier? monster-modifier? monster-modifier?)]
  [worse-modifier (-> monster-modifier? monster-modifier? monster-modifier?)]
  [absent-from-modifier-deck (-> (listof monster-modifier?) (listof monster-modifier?))]
  [monster-curse-deck (listof monster-modifier?)]
  [bless-deck (listof monster-modifier?)]
  [conditions->string (-> (listof condition?) string?)]))

(require
 racket/hash
 rebellion/type/enum
 frosthaven-manager/enum-helpers
 frosthaven-manager/constants
 frosthaven-manager/curlique)

(module+ test (require rackunit))

(define initiative? exact-integer?)

(define ability? string?)

(define-serializable-enum-type element
  (fire ice air earth light dark))

(define-constant-format/parse
 format-element parse-element
 ([fire "Fire"]
  [ice "Ice"]
  [air "Air"]
  [earth "Earth"]
  [light "Light"]
  [dark "Dark"]))

(define-serializable-enum-type monster-modifier
  (zero minus1 plus1 minus2 plus2 null crit curse bless))

(define-constant-format/parse
 format-monster-modifier parse-monster-modifier
 ([zero "+ 0"]
  [minus1 "- 1"]
  [plus1 "+ 1"]
  [minus2 "- 2"]
  [plus2 "+ 2"]
  [null "Null (x 0)"]
  [crit "Crit (x 2)"]
  [curse "Curse (x 0)"]
  [bless "Bless (x 2)"]))

(define monster-modifier-deck
  (append (build-list 6 (const zero))
          (build-list 5 (const minus1))
          (build-list 5 (const plus1))
          (list minus2 plus2 null crit)))

(define monster-curse-deck (build-list 10 (const curse)))

(define bless-deck (build-list 10 (const bless)))

(define-flow (shuffle-modifier-deck? _pulled-cards)
  (~> sep (any (one-of? null crit))))

(define modifier-rankings
  (list curse
        null
        minus2
        minus1
        zero
        plus1
        plus2
        crit
        bless))

(define modifier-ranking
  {~>> (index-of modifier-rankings)})

(define-flow (better-modifier _x _y)
  (~>> list (argmax modifier-ranking)))

(define-flow (worse-modifier _x _y)
  (~>> list (argmin modifier-ranking)))

(define (absent-from-modifier-deck cards)
  (define default-modifier-deck-counter (counter monster-modifier-deck))
  (define current-counter (counter cards))
  (unless (subset? (hash-keys current-counter)
                   (hash-keys default-modifier-deck-counter))
    (raise-argument-error 'absent-from-modifier-deck
                          "subset of monster-modifier-deck"
                          cards))
  (define difference
    (hash-union default-modifier-deck-counter
                current-counter
                #:combine -))
  (when (~> (difference) hash-values sep (any negative?))
    (raise-argument-error 'absent-from-modifier-deck
                          "subset of monster-modifier-deck"
                          cards))
  (append* (for/list ([(card num) (in-hash difference)])
             (build-list num (const card)))))

(module+ test
  (test-exn "absent-from-modifier-deck: fails on non-subset keys" #rx"subset"
            (thunk (absent-from-modifier-deck (list curse))))
  (test-exn "absent-from-modifier-deck: fails on too many cards" #rx"subset"
            (thunk (absent-from-modifier-deck (build-list 10 (const crit)))))
  (test-case "absent-from-modifier-deck: computes the difference from the standard modifier deck"
    (check-equal? (counter (absent-from-modifier-deck monster-modifier-deck)) (counter empty))
    (check-equal? (counter (absent-from-modifier-deck (shuffle monster-modifier-deck))) (counter empty))
    (check-equal? (counter (absent-from-modifier-deck empty)) (counter monster-modifier-deck))
    (check-equal? (counter (absent-from-modifier-deck
                            (shuffle (append (build-list 6 (const zero))
                                             (build-list 2 (const minus1))
                                             (build-list 5 (const plus1))
                                             (list plus2 null crit)))))
                  (counter (append (build-list 3 (const minus1))
                                   (list minus2))))))

(define-serializable-enum-type condition
  (regenerate ward invisible strengthen wound brittle bane poison immobilize disarm impair stun muddle))

(define-constant-format/parse
 format-condition parse-condition
 ([regenerate "Regenerate"]
  [ward "Ward"]
  [invisible "Invisible"]
  [strengthen "Strengthen"]
  [wound "Wound"]
  [brittle "Brittle"]
  [bane "Bane"]
  [poison "Poison"]
  [immobilize "Immobilize"]
  [disarm "Disarm"]
  [impair "Impair"]
  [stun "Stun"]
  [muddle "Muddle"]))

(define conditions
  (sort (list regenerate ward invisible strengthen wound brittle bane poison immobilize disarm impair stun muddle)
        string<=? #:key format-condition))

(define expirable-conditions
  (set
   invisible
   strengthen
   bane
   immobilize
   disarm
   impair
   stun
   muddle))

(define conditions->string
  {~> (sep format-condition) collect (string-join ", " #:before-last " and ")})

(define (counter xs)
  (for/fold ([h (hash)])
            ([x (in-list xs)])
    (hash-update h x add1 0)))
