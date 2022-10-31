#lang racket/base

(provide
  translate-to-top-coords)

(require racket/class)

(define (translate-to-top-coords this top x y)
  (let loop ([x x] [y y] [container (send this get-parent)])
    (cond
      [(not top) (values #f #f)]
      [(eq? top container) (values x y)]
      [else (loop (+ x (send container get-x))
                  (+ y (send container get-y))
                  (send container get-parent))])))
