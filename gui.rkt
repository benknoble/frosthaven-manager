#lang racket

(provide render-manager)

(require racket/gui/easy
         "defns.rkt")

(define (render-manager)
  (render
    (window
      (text (~a "hello world" " " curse)))))
