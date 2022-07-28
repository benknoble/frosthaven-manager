#lang racket

(provide
  (contract-out
    [static-table (->* ((listof label-string?)
                        natural-number/c
                        (listof (-> any/c any/c)))
                       (#:selection (maybe-obs/c
                                      (or/c #f
                                            exact-nonnegative-integer?
                                            (listof exact-nonnegative-integer?)))
                        #:widths (maybe-obs/c
                                   (or/c #f
                                         (listof
                                           (or/c (list/c exact-nonnegative-integer?
                                                         dimension-integer?)
                                                 (list/c exact-nonnegative-integer?
                                                         dimension-integer?
                                                         dimension-integer?
                                                         dimension-integer?)))))
                        #:index->entry (-> natural-number/c natural-number/c)
                        #:entry->value (-> natural-number/c any/c))
                       (is-a?/c view<%>))]))

(require (only-in racket/gui label-string? dimension-integer?)
         racket/gui/easy
         racket/gui/easy/contract
         "../qi.rkt")

;; index->entry computes the "row index", which is always the first column
;; entry->value computes the value that each function in entry->columns will be
;; given; the result of each function is stringified by ~a.
(define (static-table columns
                      num-rows
                      entry->columns
                      #:selection [selection #f]
                      #:widths [widths #f]
                      #:index->entry [index->entry values]
                      #:entry->value [entry->value values])
  (define column-widths (or widths
                            (for/list ([(e i) (in-indexed columns)])
                              (list i (* 10 (string-length e))))))
  (define (make-row i)
    (define v (entry->value i))
    (for/vector #:length (add1 (length entry->columns)) ([f (in-list (cons (const i) entry->columns))])
      (~a (f v))))
  (table
    columns
    (for/vector #:length num-rows ([i (in-range num-rows)])
      (index->entry i))
    #:selection selection
    #:min-size (list (~> (column-widths) sep (>< second) (+ 40))
                     (* 30 num-rows))
    #:column-widths column-widths
    #:stretch '(#f #f)
    #:entry->row make-row))
