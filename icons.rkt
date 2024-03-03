#lang racket

(provide
  (contract-out
   [target (-> pict?)]))

(require pict
         pict/color
         frosthaven-manager/qi)

(define (target)
  (~> (20 40)
      (>< (circle #:border-width 5))
      cc-superimpose))

(module+ main
  (require racket/gui)
  (for ([p (list target)])
    (show-pict (p))))
