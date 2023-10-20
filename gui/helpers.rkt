#lang racket/base

(provide
  translate-to-top-coords
  escape-text)

(require racket/class)

(define (translate-to-top-coords this top x y)
  (define-values (xs ys) (send this client->screen x y))
  (send top screen->client xs ys))

(define (escape-text s)
  (regexp-replace* #px"&(?!&)" s "&&"))
