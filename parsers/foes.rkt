#lang racket
; vim: lw-=do

(provide
  foes/pc
  foe/pc
  (contract-out
    [parse-foes (-> any/c input-port? #:syntax? any/c
                    (or/c syntax? foes/pc))]
    [foes/p (parser/c char? foes/pc)]
    [foe/p (parser/c char? foe/pc)]))

(require megaparsack
         megaparsack/text
         data/monad
         data/applicative
         (rename-in data/functor
                    [map fmap])
         frosthaven-manager/defns
         frosthaven-manager/qi
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

(define (opt/p p)
  (do (char/p #\() skip-ws
      [x <- p] skip-ws
      (char/p #\))
      (pure x)))

(struct labelled [label v])
(define (labelled/p label/p p)
  (do (char/p #\[) skip-ws
      [label <- label/p] skip-ws
      [v <- p] skip-ws
      (char/p #\])
      (pure (labelled label v))))

(define (map/p key/p value/p)
  (do (char/p #\<) skip-ws
      [kvps <- (many/p (labelled/p key/p value/p) #:sep skip-ws)] skip-ws
      (char/p #\>)
      (pure (~> (kvps) sep (>< (-< labelled-label labelled-v)) hash))))

(define monster-type/pc (or/c "absent" "normal" "elite"))
(define monster-type/p
  (or/p (string/p "absent") (string/p "normal") (string/p "elite")))

(define text/p
  (label/p "text"
           (do (char/p #\")
               [text <- (fmap list->string (many/p (char-not/p #\")))]
               (char/p #\")
               (pure text))))

(define non-empty-string? (not/c (string-len/c 1)))
(define (non-empty-text/p why)
  (guard/p text/p non-empty-string? why))

(define monster-name/p (non-empty-text/p "non-empty monster name"))

(define numbering/pc (or/c "ordered" "random" #f))
(define numbering/p
  (opt/p (do [option <- (or/p (string/p "ordered") (string/p "random"))] skip-ws
             (string-ci/p "numbering") skip-ws
             (pure option))))

(define spec/pc (hash/c (or/c 2 3 4) monster-type/pc #:immutable #t))
(define spec/p
  (guard/p
    (fmap
      (flow (hash-map/copy (flow (== (~> string string->number) _))))
      (map/p (char-in/p "234") monster-type/p))
    (flow (~> hash-keys (set=? '(2 3 4))))
    "entries for each of 2, 3, and 4 players"
    (flow (~>> hash-keys (set-subtract '(2 3 4))
               (map ~a) (string-join _ ", " #:before-first "missing ")))))

(define-flow name->set (~> string-split (and (~> length (> 1)) last)))

(define foe/pc (list/c string? string? numbering/pc (listof spec/pc)))
(define foe/p
  (do (string/p "begin-foe") skip-ws
      [name <- monster-name/p] skip-ws
      [set-name <- (or/p (opt/p (non-empty-text/p "non-empty set name"))
                         (guard/p
                           (~> (name) name->set pure)
                           (flow (and _ (~> string-length (not zero?))))
                           "name with monster set"
                           (const name)))] skip-ws
      [numbering <- (or/p numbering/p (pure #f))] skip-ws
      [specs <- (fmap first (many+-until/p spec/p #:sep skip-ws #:end (try/p (do skip-ws (string/p "end-foe")))))]
      (pure (list set-name name numbering specs))))

(define foes/pc
  (listof (or/c (list/c 'import string?)
                monster-info?
                (listof monster-ability?)
                foe/pc)))

(define foes/p
  (fmap first
        (do skip-ws
            (many-until/p (or/p import-monsters/p
                                (try/p monster/p)
                                (try/p ability-deck/p)
                                foe/p)
                          #:sep skip-ws
                          #:end (try/p (do skip-ws eof/p))))))

(define (parse-foes src in #:syntax? syntax?)
  (define wrapper (if syntax? syntax/p identity))
  (parse-result! (parse-string (wrapper foes/p) (port->string in) src)))
