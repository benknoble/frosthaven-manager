#lang racket
; vim: lw-=do

(provide
  (contract-out
    [bestiary/c flat-contract?]
    [parse-bestiary (-> any/c input-port? #:syntax? any/c
                        (or/c syntax? bestiary/c))]
    [monster/p (parser/c char? monster-info?)]
    [ability-deck/p (parser/c char? (listof monster-ability?))]
    [import-monsters/p (parser/c char? (list/c 'import string?))]
    [bestiary/p (parser/c char? bestiary/c)]
    [bestiary-dupes (-> (listof any/c) (values (or/c #f (listof string?))
                                               (or/c #f (listof string?))))]))

(require frosthaven-manager/parsers/base
         frosthaven-manager/defns)

(module+ test (require rackunit))

(define bestiary/c
  (listof (or/c (list/c 'import string?)
                monster-info?
                (listof monster-ability?))))

#| monster syntax
- outside of <text> elements, whitespace is ignored

"begin-monster"
  <name:text> ( "(" <set:text> ")" )? ;=> set in parens; if omitted, use last "word" of name
  ( "[" <level:number> ("elite" | "normal") <stats> "]" )16 ;=> exactly one of each of {0-7}x{"elite", "normal"}
"end-monster"

- each of the following is labelled "[" label value "]"
- lists in "{" value … "}"
- order of these doesn't matter
- Bonuses, Effects, Immunities optional (default null)
<stats> ::= HP | Move | Attack | Bonuses | Effects | Immunities |#

#| ability-deck syntax

"begin-ability-deck"
  <set:text>
  ( "[" <card-name:text> <initiative:number> "shuffle"? "{" <text>+ "}" "]" )8
"end-ability-deck" |#

(define monster-type/p
  (or/p (string-ci/p "normal") (string-ci/p "elite")))

(define (value/p s p)
  (labelled/p (string-ci/p s) p))

(define hp/p
  (value/p "HP"
           (or/p (non-empty-text/p "non-empty HP formula")
                 (guard/p number/p positive-integer? "positive maximum health"))))
(define move/p
  (value/p "Move"
           (or/p (do (string/p "-") (pure #f))
                 (guard/p number/p natural-number/c "base move value at least 0"))))
(define attack/p
  (value/p "Attack"
           (or/p (non-empty-text/p "non-empty Attack formula")
                 (guard/p number/p natural-number/c "base attack at least 0"))))
(define bonuses/p (value/p "Bonuses" (list-value/p (non-empty-text/p "non-empty bonus text"))))
(define effects/p (value/p "Effects" (list-value/p (non-empty-text/p "non-empty effect text"))))
(define immunities/p (value/p "Immunities" (list-value/p (non-empty-text/p "non-empty immunity text"))))

(define required-stat/ps (hash "HP" hp/p
                               "Move" move/p
                               "Attack" attack/p))
(define optional-stat/ps (hash "Bonuses" bonuses/p
                               "Effects" effects/p
                               "Immunities" immunities/p))
(define stat/ps (append (hash-values required-stat/ps) (hash-values optional-stat/ps)))

(define-flow stats-labels (map labelled-label _))
(define-flow stats-labels-sufficient (subset? (hash-keys required-stat/ps) _))

(define (stats-values->monster-stats label-values)
  (define lookup-table
    (list->hash label-values #:->key labelled-label #:->value labelled-v))
  (define (lookup key def)
    (if def
      (hash-ref lookup-table key def)
      (hash-ref lookup-table key)))
  (apply monster-stats
         (map lookup
              (list  "HP"  "Move"  "Attack"  "Bonuses"  "Effects"  "Immunities")
              (list  #f    #f      #f        empty      empty      empty))))

(define stats/p
  (let ([try-with-ws (λ (p)
                       (try/p
                        (do [v <- p] skip-ws
                            (pure v))))])
    (fmap stats-values->monster-stats
          (guard/p
           (many+/p (apply or/p (map try-with-ws stat/ps)) #:sep skip-ws)
           (flow (~> stats-labels (and (not check-duplicates) stats-labels-sufficient)))
           "exactly one each of HP, Move, Attack, and up to one each of Bonuses, Effects, and Immunities"
           stats-labels))))

(module+ test
  (test-case "stats/p"
    (check-equal?
     (parse-result! (parse-string stats/p "[move 3] [hp 4] [attack 3]"))
     (monster-stats 4 3 3 empty empty empty))
    (check-equal?
     (parse-result! (parse-string stats/p "[move -] [hp 4] [attack 3]"))
     (monster-stats 4 #f 3 empty empty empty))))

(define cons-label (flow (~> (-< labelled-label labelled-v) cons)))

;; (list level type stats)
(define full-stats/p
  (fmap cons-label (labelled/p
                     (guard/p number/p (flow (<= 0 _ 7)) "level between 0 and 7")
                     (list/p monster-type/p stats/p #:sep skip-ws))))

(define level-x-type
  (for*/set ([level (in-inclusive-range 0 7)]
             [type (list "normal" "elite")])
    (list level type)))

(define-flow level-x-type-dupes (~> check-duplicates (and _ (take 2))))
(define-flow level-x-type-set (~> (sep (take 2)) set))

(define-flow make-stats
  (~>> (group-by second) ;; type
       (sep (sort < #:key first)) ;; level
       (if (~> 1> first second (equal? "normal")) _ X)
       (>< (map third _)) collect))

(module+ test
  (test-case "make-stats"
    ;; use dummy hashes to stand for stats
    (define in
      '([0 "normal" (hash 'hp 1)]
        [0 "elite" (hash 'hp 2)]
        [1 "normal" (hash 'hp 3)]
        [2 "normal" (hash 'hp 5)]
        [1 "elite" (hash 'hp 4)]
        [2 "elite" (hash 'hp 6)]))
    (check-equal? (make-stats in)
                  ;; normal stats
                  '([(hash 'hp 1)
                     (hash 'hp 3)
                     (hash 'hp 5)]
                    ;; elite stats
                    [(hash 'hp 2)
                     (hash 'hp 4)
                     (hash 'hp 6)]))))

(define monster/p
  (do (string/p "begin-monster") skip-ws
      [name <- (non-empty-text/p "non-empty monster name")] skip-ws
      [set-name <- (set-name?/p name)] skip-ws
      [stats <- (guard/p (repeat/p (set-count level-x-type) (do [s <- full-stats/p] skip-ws (pure s)))
                         (flow (and (not level-x-type-dupes) (~> level-x-type-set (set=? level-x-type))))
                         "exactly one set of stats for each level (0–7) and type (normal or elite)"
                         (flow (or level-x-type-dupes
                                   (~>> level-x-type-set (set-subtract level-x-type)
                                        set->list (string-join _ ",")))))] skip-ws
      (string/p "end-monster")
      (pure
        (apply monster-info
               set-name
               name
               (make-stats stats)))))

;; (list card initiative shuffle? abilities)
(define ability-card/p
  (label/p
    "ability card"
    (fmap cons-label
          (labelled/p
            (non-empty-text/p "non-empty card name")
            (list/p (guard/p number/p (flow (<= 0 _ 99)) "valid initiative between 0 and 99")
                    (or/p (try/p (fmap (const #t) (string/p "shuffle")))
                          (pure #f))
                    (guard/p (list-value/p text/p) (flow (not empty?)) "non-empty list of card abilities")
                    #:sep skip-ws)))))

(define ability-deck/p
  (do (string/p "begin-ability-deck") skip-ws
      [set <- (non-empty-text/p "non-empty set name")] skip-ws
      [cards <- (repeat/p 8 (do [v <- ability-card/p] skip-ws (pure v)))] skip-ws
      (string/p "end-ability-deck")
      (pure (map (match-lambda
                   [(list card-name initiative shuffle? abilities)
                    (monster-ability set card-name initiative abilities shuffle? #f)])
                 cards))))

(define import-monsters/p
  (do (string/p "import-monsters") skip-ws
      [imported <- (non-empty-text/p "non-empty filename")]
      (pure `(import ,imported))))

(define-flow monster-name-dupes (~> (>< monster-info-name) collect check-duplicates))
(define-flow ability-set-dupes (~> (>< (~> first monster-ability-set-name)) collect check-duplicates))
(define listof-monster-ability? (listof monster-ability?))
(define-flow bestiary-dupes
  (~> sep (partition
            [monster-info? monster-name-dupes]
            [listof-monster-ability? ability-set-dupes])))

(define bestiary/p
  (guard/p
    (ws-separated-whole-file/p (or/p import-monsters/p (try/p monster/p) ability-deck/p))
    (flow (~> bestiary-dupes none?))
    "no duplicate monsters or ability decks"
    (flow (~> bestiary-dupes (pass _) collect (string-join ",")))))

(define parse-bestiary (make-reader-like bestiary/p))
