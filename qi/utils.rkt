#lang racket

(provide
  (contract-out
    [list-remove (-> list? natural-number/c (values list? any/c))]))

(require frosthaven-manager/curlique)

(module+ test (require rackunit))

(define list-remove
  {~> split-at
      (-< (~> (== _ cdr) append)
          (~> 2> car))})

(module+ test
  (test-case "list-remove"
    (define list-remove1 {~> list-remove 1>})
    (define list-remove2 {~> list-remove 2>})
    (check-equal? (list-remove1 '(a b c) 0) '(b c))
    (check-equal? (list-remove1 '(a b c) 1) '(a c))
    (check-equal? (list-remove1 '(a b c) 2) '(a b))
    (check-equal? (list-remove2 '(a b c) 0) 'a)
    (check-equal? (list-remove2 '(a b c) 1) 'b)
    (check-equal? (list-remove2 '(a b c) 2) 'c)
    (for ([i '(-1 3 4 5 10)])
      (check-exn exn:fail? (thunk (list-remove '(a b c) i))))))
