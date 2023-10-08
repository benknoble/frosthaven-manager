#lang racket

(provide
 prompt
 (contract-out
  [prompt/c flat-contract?]
  [beginning-of time/c]
  [end-of time/c]
  [time/c flat-contract?]
  [should-do-prompt? (-> time/c natural-number/c (listof prompt/c) any/c)]))

(require racket/serialize
         syntax/parse/define
         (for-syntax racket/contract))

(define beginning-of 'beginning-of)
(define end-of 'end-of)
(define time/c (or/c 'beginning-of 'end-of))

(serializable-struct prompt-round [n] #:transparent)
(serializable-struct prompt-even-rounds () #:transparent)
(serializable-struct prompt-odd-rounds () #:transparent)
(serializable-struct prompt-every-n-rounds [n start] #:transparent)

(define round-prompt?
  (or/c prompt-round? prompt-even-rounds? prompt-odd-rounds? prompt-every-n-rounds?))

(define prompt/c (cons/c time/c round-prompt?))

(define (should-do-prompt? time current-round prompts)
  (for/or ([p prompts] #:when (equal? (car p) time))
    (match (cdr p)
      [(prompt-round n) (= current-round n)]
      [(prompt-even-rounds) (even? current-round)]
      [(prompt-odd-rounds) (odd? current-round)]
      [(prompt-every-n-rounds n start) (and (>= current-round start)
                                            (= (modulo current-round n) (modulo start n)))])))

(begin-for-syntax
 (define-splicing-syntax-class round-prompt-spec
   #:attributes (compiled)
   [pattern {~datum even} #:attr compiled #'(prompt-even-rounds)]
   [pattern {~datum odd} #:attr compiled #'(prompt-odd-rounds)]
   [pattern {~seq {~datum every} {~var n (expr/c #'natural-number/c)}
                  {~datum starting-at} {~var start (expr/c #'natural-number/c)}}
            #:attr compiled #'(prompt-every-n-rounds n.c start.c)]
   [pattern {~var n (expr/c #'natural-number/c)} #:attr compiled #'(prompt-round n.c)])

 (define-syntax-class prompt-spec
   #:attributes (compiled)
   [pattern [{~var time (expr/c #'time/c)} s:round-prompt-spec]
            #:attr compiled #'(cons time.c s.compiled)]))

(define-syntax-parse-rule (prompt s:prompt-spec ...)
  (list s.compiled ...))

(module* test racket/base
  (require (submod "..")
           rackunit
           racket/serialize)

  (let ([p (prompt [beginning-of even]
                   [beginning-of odd]
                   [beginning-of every 5 starting-at 2]
                   [beginning-of 3]
                   [end-of even]
                   [end-of odd]
                   [end-of every 7 starting-at 4]
                   [end-of 2])])
    (check-equal? (deserialize (serialize p)) p))

  (check-true (should-do-prompt? beginning-of 2 (prompt [beginning-of 2])))
  (check-false (should-do-prompt? beginning-of 2 (prompt [end-of 2])))
  (check-true (should-do-prompt? end-of 2 (prompt [end-of 2])))
  (check-false (should-do-prompt? end-of 2 (prompt [beginning-of 2])))
  (check-true (should-do-prompt? end-of 2 (prompt [end-of 1] [end-of 2] [end-of 3])))
  (check-true (should-do-prompt? beginning-of 2 (prompt [end-of odd] [beginning-of 2] [end-of every 3 starting-at 3])))
  (check-false (should-do-prompt? beginning-of 1 (prompt [beginning-of 2])))
  (check-false (should-do-prompt? end-of 2 (prompt [end-of 1])))

  (for ([i 10] #:when (even? i))
    (check-true (should-do-prompt? beginning-of i (prompt [beginning-of even])))
    (check-true (should-do-prompt? end-of i (prompt [end-of even])))
    (check-false (should-do-prompt? beginning-of i (prompt [end-of even])))
    (check-false (should-do-prompt? end-of i (prompt [beginning-of even])))
    (check-false (should-do-prompt? beginning-of i (prompt [beginning-of odd])))
    (check-false (should-do-prompt? end-of i (prompt [end-of odd])))
    (check-false (should-do-prompt? beginning-of i (prompt [end-of odd])))
    (check-false (should-do-prompt? end-of i (prompt [beginning-of odd]))))

  (for ([i 10] #:when (odd? i))
    (check-true (should-do-prompt? beginning-of i (prompt [beginning-of odd])))
    (check-true (should-do-prompt? end-of i (prompt [end-of odd])))
    (check-false (should-do-prompt? beginning-of i (prompt [end-of odd])))
    (check-false (should-do-prompt? end-of i (prompt [beginning-of odd])))
    (check-false (should-do-prompt? beginning-of i (prompt [beginning-of even])))
    (check-false (should-do-prompt? end-of i (prompt [end-of even])))
    (check-false (should-do-prompt? beginning-of i (prompt [end-of even])))
    (check-false (should-do-prompt? end-of i (prompt [beginning-of even]))))

  (for ([i 10])
    (check-true (should-do-prompt? beginning-of i (prompt [beginning-of i])))
    (check-true (should-do-prompt? end-of i (prompt [end-of i])))
    (check-true (should-do-prompt? beginning-of i (prompt [beginning-of odd] [beginning-of even])))
    (check-true (should-do-prompt? end-of i (prompt [end-of odd] [end-of even])))
    (check-true (should-do-prompt? beginning-of i (prompt [beginning-of odd] [beginning-of even]
                                                          [end-of odd] [end-of even])))
    (check-true (should-do-prompt? end-of i (prompt [beginning-of odd] [beginning-of even]
                                                    [end-of odd] [end-of even]))))

  (check-false (should-do-prompt? end-of 2 (prompt [end-of every 3 starting-at 5])))
  (check-true (should-do-prompt? end-of 8 (prompt [end-of every 3 starting-at 5])))

  (for ([i (in-range 3 10 3)])
    (check-true (should-do-prompt? beginning-of i (prompt [beginning-of every 3 starting-at 3])))
    (check-true (should-do-prompt? end-of i (prompt [end-of every 3 starting-at 3])))
    (for ([j '(1 2)])
      (check-false (should-do-prompt? beginning-of i (prompt [beginning-of every 3 starting-at j])))
      (check-false (should-do-prompt? end-of i (prompt [end-of every 3 starting-at j])))))

  (for ([i (in-range 2 10 3)])
    (check-true (should-do-prompt? beginning-of i (prompt [beginning-of every 3 starting-at 2])))
    (check-true (should-do-prompt? end-of i (prompt [end-of every 3 starting-at 2])))
    (for ([j '(1 3)])
      (check-false (should-do-prompt? beginning-of i (prompt [beginning-of every 3 starting-at j])))
      (check-false (should-do-prompt? end-of i (prompt [end-of every 3 starting-at j]))))))
