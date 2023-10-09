#lang racket

(provide
  (contract-out
    [list-remove (-> list? natural-number/c (values list? any/c))]))

(require qi)

(define-flow (list-remove _xs _i)
  (~> split-at
      (-< (~> (== _ cdr) append)
          (~> 2> car))))
