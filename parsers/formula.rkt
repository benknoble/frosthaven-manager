#lang racket
; vim: lw-=do

(provide
  (contract-out
    [env/c flat-contract?]
    [expr/pc contract?]
    [expr/p (parser/c char? expr/pc)]
    [parse-expr (-> string? expr/pc)]))

(require frosthaven-manager/parsers/base)

#| formulas syntax

<expr> ::= product ([+-] product)*
<product> ::= term ([*/] term)*
<term> ::= "(" <expr> ")" | <number> | <var>
<var> ::= "L" | "C" |#

(define env/c (hash/c (or/c "L" "C") number? #:flat? #t))
(define expr/pc (-> env/c number?))

(define var/p
  (do [var <- (or/p (string/p "L") (string/p "C"))]
    (pure (λ (env) (hash-ref env var)))))

(define num/p
  (do [num <- number/p]
    (pure (const num))))

(define bracketed-expr/p
  (fmap second
        (list/p (char/p #\() (delay/p expr/p) (char/p #\))
                #:sep skip-ws)))

(define term/p
  (or/p var/p num/p bracketed-expr/p))

(define product/p
  (do [p <- term/p] skip-ws
      [ps <- (many/p (list/p (or/p (string/p "*") (string/p "/")) term/p #:sep skip-ws))] skip-ws
      (pure
        (for/fold ([res p])
          ([op-p (in-list ps)])
          (match-define (list op p) op-p)
          (case op
            [("*") (λ (env) (* (res env) (p env)))]
            [("/") (λ (env) (/ (res env) (p env)))])))))

(define expr/p
  (do [p <- product/p] skip-ws
      [ps <- (many/p (list/p (or/p (string/p "+") (string/p "-")) product/p #:sep skip-ws))] skip-ws
      (pure
        (for/fold ([res p])
          ([op-p (in-list ps)])
          (match-define (list op p) op-p)
          (case op
            [("+") (λ (env) (+ (res env) (p env)))]
            [("-") (λ (env) (- (res env) (p env)))])))))

(define (parse-expr s)
  (parse-result! (parse-string expr/p s)))
