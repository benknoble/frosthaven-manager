#lang racket

(provide only-on-text
         match-loop)

(require syntax/parse/define)

;; f: x -> listof y
(define (only-on-text f . xs)
  (let ([f* (if (null? xs) f (apply curry f xs))])
    (λ (x)
      (cond
        [(string? x) (f* x)]
        [else (list x)]))))

(define-syntax-parser match-loop
  [(_ input:expr [pat:expr e ... res:expr] ...)
   (syntax/loc this-syntax
     (let loop ([x input])
       (match x
         [pat e ... (append-map loop res)]
         ...
         ;; break
         [_ (list x)])))])
