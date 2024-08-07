#lang racket/gui

(provide
 (contract-out
  [get-file/filter (-> label-string? (list/c string? string?) (or/c path? #f))]
  [put-file/filter (->* {label-string? (list/c string? string?)}
                        {(or/c path-string? #f) (or/c path-string? #f)}
                        (or/c path? #f))]))

(require frosthaven-manager/curlique)

(define (get-file/filter message filter)
  (get-file message #f #f #f (->extension (second filter)) empty (list filter '("Any" "*.*"))))

(define (put-file/filter message filter [directory #f] [file #f])
  (put-file message #f directory file (->extension (second filter)) empty (list filter '("Any" "*.*"))))

(define ->extension
  {~> path-get-extension (and _ (~> bytes->string/utf-8 (substring 1)))})
