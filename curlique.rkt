#lang racket

(provide (rename-out [curlique-app #%app]
                     [curlique~> ~>]
                     [curlique~>> ~>>]
                     [curlique-switch switch])
         (except-out (all-from-out qi)
                     ~>
                     ~>>
                     switch))

(require (only-in racket [#%app racket:app])
         syntax/parse/define
         (for-syntax syntax/parse/class/paren-shape)
         qi)

(define-syntax curlique-app
  (syntax-parser
   [{~braces _ x ...}
    (syntax/loc this-syntax
      (flow x ...))]
   [(_ x ...)
    (syntax/loc this-syntax
      (racket:app x ...))]))

(define-syntax-parse-rule (define-curlique-syntax name:id qi-form:id)
  #:with ooo #'(... ...)
  (define-syntax name
    (syntax-parser
     [{~braces _ x ooo}
      (syntax/loc this-syntax
        (flow (qi-form x ooo)))]
     [(_ x ooo)
      (syntax/loc this-syntax
        (qi-form x ooo))])))

(define-syntax-parse-rule (define-curlique-syntaxes [name:id qi-form:id] ...)
  (begin (define-curlique-syntax name qi-form) ...))

(define-curlique-syntaxes
 [curlique~> ~>]
 [curlique~>> ~>>]
 [curlique-switch switch])

(module* test racket
  (require (submod ".."))
  (require rackunit
           syntax/macro-testing)
  ;; flow errors
  (check-exn #rx"more terms" (thunk (convert-syntax-error {(><)})))
  (check-exn #rx"flow" (thunk (convert-syntax-error {list 1 2 2})))
  ;; handy identity
  (check-equal? {} values)
  ;; macros bypass #%app
  (check-equal? {on (1)} 1)
  (check-equal? ({thunk 123}) 123)
  ;; flow and curlique-syntax examples
  (check-equal? (map {~>} (range 10)) (range 10))
  (check-equal? ({~> (-< add1 sub1) /} 5) 3/2)
  (check-equal? (~> (1 2 3) +) 6)
  (check-equal? (map {switch [positive? add1] [negative? sub1]}
                     (inclusive-range -2 2))
                '(-3 -2 0 2 3))
  (check-true ({(all positive?)} 1 2 3 4)))
