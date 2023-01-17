#lang racket
; vim: lw-=do

;; TODO: make syntaxes more consistent

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
  (do (char/p #\[) skip-ws
      [option <- (or/p (string/p "ordered") (string/p "random"))] skip-ws
      (string-ci/p "numbering") skip-ws
      (char/p #\])
      (pure option)))

(define spec/pc (list/c monster-type/pc monster-type/pc monster-type/pc))
(define spec/p
  (let ([spec-part (λ (n)
                     (do (string/p (~a n ":"))
                         monster-type/p))])
    (do (char/p #\<) skip-ws
        [2p <- (spec-part 2)] skip-ws
        [3p <- (spec-part 3)] skip-ws
        [4p <- (spec-part 4)] skip-ws
        (char/p #\>)
        (pure (list 2p 3p 4p)))))

(define-flow name->set (~> string-split (and (~> length (> 1)) last)))

(define foe/pc (list/c string? string? numbering/pc (listof spec/pc)))
(define foe/p
  (do (string/p "begin-foe") skip-ws
      [name <- monster-name/p] skip-ws
      [set-name <- (or/p (do (char/p #\() skip-ws
                             [set <- (non-empty-text/p "non-empty set name")] skip-ws
                             (char/p #\))
                             (pure set))
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
