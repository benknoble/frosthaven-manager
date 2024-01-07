#lang racket

(provide
  (contract-out
    [counter (-> (maybe-obs/c string?)
                 (-> any)
                 (-> any)
                 (is-a?/c view<%>))]))

(require racket/gui/easy
         racket/gui/easy/contract)

(define (counter @label up down)
  (hpanel #:stretch '(#f #f)
          (button "-" down)
          (text @label)
          (button "+" up)))
