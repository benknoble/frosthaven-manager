#lang racket

(provide render-manager)

(require racket/gui/easy)

(define (render-manager)
  (render
    (window
      (text "hello world"))))
