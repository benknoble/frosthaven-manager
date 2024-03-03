#lang racket

(provide
  (contract-out
   [target (-> pict?)]
   [range (-> pict?)]))

(require pict
         pict/color
         frosthaven-manager/qi
         frosthaven-manager/aoe-images)

(define (target)
  (~> (20 40)
      (>< (circle #:border-width 5))
      cc-superimpose))

(define (range)
  (parameterize ([hex-size 10])
    (define left (S))
    (define right (ghost (S)))
    (~> (left right)
        hc-append
        (pin-arrow-line
         5
         _
         left cc-find
         right cc-find)
        (inset 0 0 -10 0))))

(module+ main
  (require (only-in racket/gui))
  (for ([p (list target
                 range)])
    (show-pict (p))))
