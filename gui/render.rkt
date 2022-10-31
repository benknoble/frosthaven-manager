#lang racket/base

(require racket/contract
         racket/gui/easy)
(provide
  (contract-out
    [current-renderer (parameter/c (or/c #f renderer?))]))

(define current-renderer (make-parameter #f))
