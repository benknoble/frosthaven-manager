#lang racket

(provide
 (enum-out element)
 (enum-out monster-modifier)
 (enum-out condition)
 (contract-out
  [initiative? predicate/c]
  [ability? predicate/c]
  [conditions (listof condition?)]
  [discriminator:condition (-> condition? integer?)]
  [selector:condition (-> integer? condition?)]
  [monster-modifier-deck (listof monster-modifier?)]
  [shuffle-modifier-deck? (-> (listof monster-modifier?) boolean?)]
  [better-modifier (-> monster-modifier? monster-modifier? monster-modifier?)]
  [worse-modifier (-> monster-modifier? monster-modifier? monster-modifier?)]
  [monster-curse-deck (listof monster-modifier?)]
  [bless-deck (listof monster-modifier?)]))

(require
 rebellion/type/enum
 frosthaven-manager/enum-helpers
 frosthaven-manager/qi)

(define initiative? (integer-in 0 99))

(define ability? string?)

(define-serializable-enum-type element
  (fire ice air earth light dark)
  #:property-maker make-property-maker-that-displays-as-constant-names)

(define-serializable-enum-type monster-modifier
  (zero minus1 plus1 minus2 plus2 null crit curse bless)
  #:property-maker make-property-maker-that-displays-as-constant-names)

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

(define-flow (modifier-ranking _mod)
  (~>> (index-of modifier-rankings)))

(define-flow (better-modifier _x _y)
  (~>> list (argmax modifier-ranking)))

(define-flow (worse-modifier _x _y)
  (~>> list (argmin modifier-ranking)))

(define-serializable-enum-type condition
  (regenerate ward invisible strengthen wound brittle bane poison immobilize disarm impair stun muddle)
  #:property-maker make-property-maker-that-displays-as-constant-names)

(define conditions
  (sort (list regenerate ward invisible strengthen wound brittle bane poison immobilize disarm impair stun muddle)
        string<=? #:key ~a))
