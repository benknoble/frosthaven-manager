#lang racket

(provide (for-space qi partition))

(require qi
         (for-syntax syntax/parse))

(define-qi-syntax-parser partition-inefficient
  [(_) #'ground]
  [(_ [cond? body]) #'(~> (pass cond?) body)]
  [(_ [cond? body] [cond?s bodys] ...)
   #'(sieve cond? body (partition-inefficient [cond?s bodys] ...))])

(define (partition-values c+bs . args)
  (define acc0
    (for/hasheq ([c+b (in-list c+bs)])
      (values (car c+b) empty)))
  (define by-cs
    (for/fold ([acc acc0]
               #:result (for/hash ([(c args) (in-hash acc)])
                          (values c (reverse args))))
      ([arg (in-list args)])
      (define matching-c
        (for*/first ([c+b (in-list c+bs)]
                     [c (in-value (car c+b))]
                     #:when (c arg))
          c))
      (if matching-c
        (hash-update acc matching-c (flow (cons arg _)))
        acc)))
  (define results
    (for*/list ([c+b (in-list c+bs)]
                [c (in-value (car c+b))]
                [b (in-value (cdr c+b))]
                [args (in-value (hash-ref by-cs c))])
      (call-with-values (thunk (apply b args)) list)))
  (apply values (apply append results)))

(define-qi-syntax-parser partition
  [(_) #'ground]
  [(_ [cond? body]) #'(~> (pass cond?) body)]
  [(_ [cond? body] ...+)
   #:with c+bs #'(list (cons (flow cond?) (flow body)) ...)
   #'(~>> (partition-values c+bs))])

(module+ test
  (require rackunit)
  ;; ground
  (check-true (~> () (partition-inefficient) (not live?)))
  (check-true (~>> () (partition-values (list)) (not live?)))
  (check-true (~> () (partition) (not live?)))
  ;; pass
  (check-equal? (~> (-1 2 1 1 -2 2)
                    (partition-inefficient [positive? +]))
                6)
  (check-equal? (partition-values (list (cons positive? +))
                                  -1 2 1 1 -2 2)
                6)
  (check-equal? (~> (-1 2 1 1 -2 2)
                    (partition [positive? +]))
                6)
  ;; generalized
  (check-equal? (~> (-1 0 2 1 1 -2 0 0 2)
                    (partition-inefficient [positive? +]
                                           [zero? (-< count (gen "zero"))]
                                           [negative? *])
                    collect)
                (list 6 3 "zero" 2))
  (check-equal?
    (call-with-values
      (thunk (partition-values
               (list (cons positive? +)
                     (cons zero? (flow (-< count (gen "zero"))))
                     (cons negative? *))
               -1 0 2 1 1 -2 0 0 2))
      list)
    (list 6 3 "zero" 2))
  (check-equal? (~> (-1 0 2 1 1 -2 0 0 2)
                    (partition [positive? +]
                               [zero? (-< count (gen "zero"))]
                               [negative? *])
                    collect)
                (list 6 3 "zero" 2))
  ;; some bodys have no input
  (check-equal? (~> (-1 2 1 1 -2 2)
                    (partition-inefficient [positive? +]
                                           [zero? (-< count (gen "zero"))]
                                           [negative? *])
                    collect)
                (list 6 0 "zero" 2))
  (check-equal?
    (call-with-values
      (thunk (partition-values
               (list (cons positive? +)
                     (cons zero? (flow (-< count (gen "zero"))))
                     (cons negative? *))
               -1 2 1 1 -2 2))
      list)
    (list 6 0 "zero" 2))
  (check-equal? (~> (-1 2 1 1 -2 2)
                    (partition [positive? +]
                               [zero? (-< count (gen "zero"))]
                               [negative? *])
                    collect)
                (list 6 0 "zero" 2))
  ;; flow in the cond?
  (check-equal? (~> (-1 2 1 1 -2 2)
                    (partition-inefficient [(and positive? (> 1)) +]
                                           [_ list])
                    collect)
                (list 4 (list -1 1 1 -2)))
  (check-equal?
    (call-with-values
      (thunk
        (partition-values
          (list (cons (flow (and positive? (> 1))) +)
                [cons (flow _) list])
          -1 2 1 1 -2 2))
      list)
    (list 4 (list -1 1 1 -2)))
  (check-equal? (~> (-1 2 1 1 -2 2)
                    (partition [(and positive? (> 1)) +]
                               [_ list])
                    collect)
                (list 4 (list -1 1 1 -2))))
