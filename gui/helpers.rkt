#lang racket/base

(provide
  translate-to-top-coords
  escape-text)

(require racket/class)

(module+ test (require rackunit))

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
