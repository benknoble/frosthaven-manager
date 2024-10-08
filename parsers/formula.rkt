#lang racket
; vim: lw-=do

(provide
  (contract-out
    [env/c flat-contract?]
    [expr/pc contract?]
    [expr/p (parser/c char? expr/pc)]
    [parse-expr (-> string? expr/pc)]))

(require frosthaven-manager/parsers/base)

(module+ test (require rackunit))

#| formulas syntax

<expr> ::= product ([+-] product)*
<product> ::= term ([*/] term)*
<term> ::= "up(" <expr> ") | "down(" <expr> ")" | "(" <expr> ")" | <number> | <var>
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

(define up/p
  (do (string/p "up(")
      [e <- expr/p]
      (string/p ")")
      (pure (λ (env) (exact-ceiling (e env))))))

(define down/p
  (do (string/p "down(")
      [e <- expr/p]
      (string/p ")")
      (pure (λ (env) (exact-floor (e env))))))

(define term/p
  (or/p var/p num/p up/p down/p bracketed-expr/p))

(define product/p
  (do [p <- term/p] skip-ws
      [ps <- (many/p (list/p (or/p (string/p "*") (string/p "/")) term/p #:sep skip-ws) #:sep skip-ws)] skip-ws
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

(define whole-string-expr/p
  (do [p <- expr/p]
      eof/p
      (pure p)))

(define (parse-expr s)
  (parse-result! (parse-string whole-string-expr/p s)))

(module+ test
  (test-case "parse-expr"
    (check-equal? ((parse-expr "5-3-1") (hash)) 1)
    (check-equal? ((parse-expr "30/15/5") (hash)) 2/5)
    (check-equal? ((parse-expr "5*6-2") (hash)) 28)
    (check-equal? ((parse-expr "5 + 3 - 1") (hash)) 7)
    (check-equal? ((parse-expr "down(4 * 3 / 2)") (hash)) 6)
    (check-equal? ((parse-expr "4 * 3 / 2") (hash)) 6)
    (check-exn exn:fail:read:megaparsack? (thunk (parse-expr "(3*4))")))
    (check-exn exn:fail:read:megaparsack? (thunk (parse-expr "down((C-1)/2) * (L+3))")))))
