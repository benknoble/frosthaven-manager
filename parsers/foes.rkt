#lang racket
; vim: lw-=do

(provide
  (contract-out
    [foes/pc flat-contract?]
    [foe/pc flat-contract?]
    [spec/pc flat-contract?]
    [numbering/pc flat-contract?]
    [monster-type/pc flat-contract?]
    [parse-foes (-> any/c input-port? #:syntax? any/c
                    (or/c syntax? foes/pc))]
    [foes/p (parser/c char? foes/pc)]
    [foe/p (parser/c char? foe/pc)]))

(require frosthaven-manager/defns
         frosthaven-manager/parsers/base
         frosthaven-manager/parsers/monster)

#| foes syntax
- outside of <text> elements, whitespace is ignored

<top> ::= (<foe> | <monster> | <import>)*

<foe> ::= "begin-foe"
            <monster-name:text> ("(" <set:text> ")")?
            ("[" ("ordered"|"random") "numbering" "]")?
            <spec>+
          "end-foe"

<spec> ::= "<" "2:"<type> "3:"<type> "4:"<type> ">"

<type> ::= "absent" | "normal" | "elite"

<import>, <monster>, <monster-name> exactly like in bestiary |#

(define monster-type/pc (or/c "absent" "normal" "elite"))
(define monster-type/p
  (or/p (string/p "absent") (string/p "normal") (string/p "elite")))

(define monster-name/p (non-empty-text/p "non-empty monster name"))

(define numbering/pc (or/c "ordered" "random" #f))
(define numbering/p
  (opt/p (do [option <- (or/p (string/p "ordered") (string/p "random"))] skip-ws
             (string-ci/p "numbering") skip-ws
             (pure option))))

(define spec/pc (hash/c num-players/c monster-type/pc #:immutable #t))
(define valid-player-nums (inclusive-range 2 max-players))
(define spec/p
  (guard/p
    (fmap
      {(hash-map/copy {(== (~> string string->number) _)})}
      (map/p (char-in/p "234") monster-type/p))
    {~> hash-keys (set=? valid-player-nums)}
    "entries for each of 2, 3, and 4 players"
    {~>> hash-keys (set-subtract valid-player-nums)
         (map ~a) (string-join _ ", " #:before-first "missing ")}))

(define foe/pc (list/c string? string? numbering/pc (listof spec/pc)))
(define foe/p
  (do (string/p "begin-foe") skip-ws
      [name <- monster-name/p] skip-ws
      [set-name <- (set-name?/p name)] skip-ws
      [numbering <- (or/p numbering/p (pure #f))] skip-ws
      [specs <- (fmap first (many+-until/p spec/p #:sep skip-ws #:end (try/p (do skip-ws (string/p "end-foe")))))]
      (pure (list set-name name numbering specs))))

(define foes/pc
  (list/c (cons/c 'import (listof string?))
          (cons/c 'info (listof monster-info?))
          (cons/c 'ability (listof monster-ability?))
          (cons/c 'foe (listof foe/pc))))

(define-flow foe-dupes
  (~> sep (partition
           [foe/pc (~> (>< second) collect check-duplicates)])))

(define big-bag-foes/p
  (guard/p
    (ws-separated-whole-file/p (or/p import-monsters/p
                                     (try/p monster/p)
                                     (try/p ability-deck/p)
                                     foe/p))
    {(and (~> bestiary-dupes none?)
          (~> foe-dupes none?))}
    "no duplicate monsters, ability decks, or foes"
    {~> (-< bestiary-dupes foe-dupes) (pass _) collect (string-join ",")}))

(define-flow big-bag->structured
  (~> sep
      (partition
       [(esc (list/c 'import string?)) (~>> (>< second) collect (cons 'import))]
       [monster-info? (~>> collect (cons 'info))]
       [(esc (listof monster-ability?)) (~>> (>< sep) collect (cons 'ability))]
       [foe/pc (~>> collect (cons 'foe))])
      collect))

(define foes/p (fmap big-bag->structured big-bag-foes/p))

(define parse-foes (make-reader-like foes/p))
