#lang racket

(provide
  translate-to-top-coords
  escape-text
  define-error-text)

(require racket/class
         syntax/parse/define
         racket/gui/easy/operator)

(module+ test (require rackunit
                       racket/gui/easy
                       syntax/macro-testing))

(define (translate-to-top-coords this top x y)
  (define-values (xs ys) (send this client->screen x y))
  (send top screen->client xs ys))

(define (escape-text s)
  (regexp-replace* #px"&(?!&)" s "&&"))

(module+ test
  (test-case "escape-text"
    (check-equal? (escape-text "foo") "foo")
    (check-equal? (escape-text "Flourish & Fletch & Foo") "Flourish && Fletch && Foo")
    (check-equal? (escape-text "Flourish && Fletch") "Flourish &&& Fletch")))

(define-syntax define-error-text
  (syntax-parser
   [(_ @error-text:id with-error-text:id)
    #:fail-when (equal? 'expression (syntax-local-context)) "not allowed in an expression context"
    (syntax/loc this-syntax
      (begin
        (define/obs @error-text "")
        (define-syntax with-error-text (-with-error-text #'@error-text))))]))

(define-for-syntax (-with-error-text error-text-id)
  (syntax-parser
   [(_ e:expr ...+)
    (quasisyntax/loc this-syntax
      (call-with-error-text #,error-text-id (thunk e ...)))]))

(define (call-with-error-text @error-text th)
  (:= @error-text "")
  (with-handlers ([exn:fail? (λ (e) (:= @error-text (exn-message e)))])
    (th)))

(module+ test
  (let ()
    (define-error-text @e wet)
    (check-equal? (obs-peek @e) "")
    (check-equal? (wet (add1 2)) 3)
    (check-equal? (obs-peek @e) "")
    (check-not-exn (thunk (wet (/ 1 0))))
    (check-regexp-match #rx"division by zero" (obs-peek @e)))
  (check-exn #rx"expression context"
             (thunk
              (convert-syntax-error (if 1 (define-error-text @x wxt) 2)))))
