#lang racket
; vim: lw-=do

(provide
  (contract-out
    [parse-bestiary (-> any/c input-port? #:syntax? any/c
                        (or/c syntax? (listof (or/c monster-info? (listof monster-ability?)))))]
    [monster/p (parser/c char? monster-info?)]
    [ability-deck/p (parser/c char? (listof monster-ability?))]
    [bestiary/p (parser/c char? (listof (or/c monster-info? (listof monster-ability?))))]))

(require megaparsack
         megaparsack/text
         data/monad
         data/applicative
         (rename-in data/functor
                    [map fmap])
         frosthaven-manager/qi
         frosthaven-manager/defns)

#| monster syntax
- outside of <text> elements, whitespace is ignored

"begin-monster"
  <name:text> ( "(" <set:text> ")" )? ;=> set in parens; if omitted, use second "word" of name
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

;; megaparsack's string-ci/p is broken
;; (https://github.com/lexi-lambda/megaparsack/issues/24)
(define (string-ci/p s)
  (if (zero? (string-length s))
    (pure "")
    (label/p s (do (char-ci/p (string-ref s 0))
                   (string-ci/p (substring s 1))
                   (pure s)))))

(define skip-ws
  (do (many/p space/p)
      void/p))

(define (list-value/p p)
  (do (char/p #\{) skip-ws
      [v <- (many/p p #:sep skip-ws)] skip-ws
      (char/p #\})
      (pure v)))

(struct labelled [label v])
(define (value/p label p)
  (do (char/p #\[) skip-ws
      (string-ci/p label) skip-ws
      [v <- p] skip-ws
      (char/p #\])
      (pure (labelled label v))))

(define monster-type/p
  (or/p (string-ci/p "normal") (string-ci/p "elite")))

(define text/p
  (label/p "text"
           (do (char/p #\")
               [text <- (fmap list->string (many/p (char-not/p #\")))]
               (char/p #\")
               (pure text))))

(define number/p
  (label/p "number"
           (do [sign <- (or/p (fmap (const -) (char/p #\-))
                              (pure +))]
               [number <- integer/p]
               (pure (sign number)))))

(define non-empty-string? (not/c (string-len/c 1)))

(define hp/p (value/p "HP" (guard/p number/p positive-integer? "positive maximum health")))
(define move/p (value/p "Move" (guard/p number/p natural-number/c "base move value at least 0")))
(define attack/p (value/p "Attack" (guard/p number/p natural-number/c "base attack at least 0")))
(define bonuses/p (value/p "Bonuses" (list-value/p (guard/p text/p non-empty-string? "non-empty bonus text"))))
(define effects/p (value/p "Effects" (list-value/p (guard/p text/p non-empty-string? "non-empty effect text"))))
(define immunities/p (value/p "Immunities" (list-value/p (guard/p text/p non-empty-string? "non-empty immunity text"))))

(define required-stat/ps (hash "HP" hp/p
                               "Move" move/p
                               "Attack" attack/p))
(define optional-stat/ps (hash "Bonuses" bonuses/p
                               "Effects" effects/p
                               "Immunities" immunities/p))
(define stat/ps (append (hash-values required-stat/ps) (hash-values optional-stat/ps)))

(define stats/p
  (fmap (λ (label-values)
          (define lookup-table
            (for/hash ([lv (in-list label-values)])
              (values (labelled-label lv) (labelled-v lv))))
          (define (lookup key def)
            (if def
              (hash-ref lookup-table key def)
              (hash-ref lookup-table key)))
          (apply monster-stats
                 (map lookup
                      (list  "HP"  "Move"  "Attack"  "Bonuses"  "Effects"  "Immunities")
                      (list  #f    #f      #f        empty      empty      empty))))
        (guard/p
          (many+/p (apply or/p (map try/p stat/ps)) #:sep skip-ws)
          (flow (~>> (map labelled-label)
                     (and (not check-duplicates)
                          (subset? (hash-keys required-stat/ps) _))))
          "exactly one each of HP, Move, Attack, and up to one each of Bonuses, Effects, and Immunities"
          (flow (map labelled-label _)))))

(define full-stats/p
  (do (char/p #\[) skip-ws
      [level <- (guard/p number/p (flow (<= 0 _ 7)) "level between 0 and 7")] skip-ws
      [type <- monster-type/p] skip-ws
      [stats <- stats/p] skip-ws
      (char/p #\])
      (pure (list level type stats))))

(define level-x-type
  (for*/set ([level (in-inclusive-range 0 7)]
             [type (list "normal" "elite")])
    (list level type)))

(define monster/p
  (do (string/p "begin-monster") skip-ws
      [name <- (guard/p text/p non-empty-string? "non-empty monster name")] skip-ws
      [set-name <- (or/p (do (char/p #\() skip-ws
                             [set <- (guard/p text/p non-empty-string? "non-empty set name")] skip-ws
                             (char/p #\))
                             (pure set))
                         (guard/p
                           (~> (name) string-split (and (~> length (> 1)) last) pure)
                           (flow (and _ (~> string-length (not zero?))))
                           "name with monster set"
                           (const name)))] skip-ws
      [stats <- (guard/p (repeat/p (set-count level-x-type) (do [s <- full-stats/p] skip-ws (pure s)))
                         (flow (and (not check-duplicates)
                                    (~>> (sep (take 2)) set (set=? level-x-type))))
                         "exactly one set of stats for each level (0–7) and type (normal or elite)"
                         (flow (or (~> check-duplicates (and _ (take 2)))
                                   (~>> (sep (take 2)) set (set-subtract level-x-type)
                                        set->list (string-join _ ",")))))] skip-ws
      (string/p "end-monster")
      (pure
        (apply monster-info
               set-name
               name
               (~>> (stats)
                    (group-by second) ;; type
                    (sep (sort < #:key first)) ;; level
                    (if (~> 1> first second (equal? "normal")) _ X)
                    (>< (map third _)) collect)))))

(define ability-card/p
  (label/p
    "ability card"
    (do (char/p #\[) skip-ws
        [card <- (guard/p text/p non-empty-string? "non-empty card name")] skip-ws
        [initiative <- (guard/p number/p (flow (<= 0 _ 99)) "valid initiative between 0 and 99")] skip-ws
        [shuffle? <- (or/p (try/p (fmap (const #t) (string/p "shuffle")))
                           (pure #f))] skip-ws
        [abilities <- (guard/p (list-value/p text/p) (flow (not empty?)) "non-empty list of card abilities")] skip-ws
        (char/p #\])
        (pure (list card initiative shuffle? abilities)))))

(define ability-deck/p
  (do (string/p "begin-ability-deck") skip-ws
      [set <- (guard/p text/p non-empty-string? "non-empty set name")] skip-ws
      [cards <- (repeat/p 8 (do [v <- ability-card/p] skip-ws (pure v)))] skip-ws
      (string/p "end-ability-deck")
      (pure (map (match-lambda
                   [(list card-name initiative shuffle? abilities)
                    (monster-ability set card-name initiative abilities shuffle?)])
                 cards))))

(define bestiary/p
  (guard/p
    (fmap first
          (do skip-ws
              (many-until/p (or/p (try/p monster/p) ability-deck/p)
                            #:sep skip-ws
                            #:end (try/p (do skip-ws eof/p)))))
    (flow (~> sep
              (partition
                [monster-info? (~> (>< monster-info-name) collect (not check-duplicates))]
                [(and list? (andmap monster-ability? _))
                 (~> (>< (~> first monster-ability-set-name)) collect (not check-duplicates))])
              AND))
    "no duplicate monster or ability-decks"
    (flow (~> sep
              (partition
                [monster-info? (~> (>< monster-info-name) collect check-duplicates)]
                [(and list? (andmap monster-ability? _))
                 (~> (>< (~> first monster-ability-set-name)) collect check-duplicates)])
              (pass _) collect (string-join ",")))))

(define (parse-bestiary src in #:syntax? syntax?)
  (define wrapper (if syntax? syntax/p identity))
  (parse-result! (parse-string (wrapper bestiary/p) (port->string in) src)))
