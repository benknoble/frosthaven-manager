#lang racket/base

(provide
  translate-to-top-coords)

(require racket/class)

(define (translate-to-top-coords this top x y)
  (define-values (xs ys) (send this client->screen x y))
  (send top screen->client xs ys))
