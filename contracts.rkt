#lang racket

(provide
 (contract-out
  [unique-with/c (-> (-> any/c any/c) flat-contract? contract?)]))

(require
 frosthaven-manager/qi)

(define-flow no-duplicates?
  (not (and check-duplicates #t)))

(define (unique/c c)
  (flat-named-contract
    'unique/c
    (and/c (listof c) no-duplicates?)))

(define (unique-with/c key c)
  (define (ctc xs)
    ((unique/c c) (map key xs)))
  (flat-named-contract
    (list 'unique-with/c (object-name key) (object-name c))
    ctc))
