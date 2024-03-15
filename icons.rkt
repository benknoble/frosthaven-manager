#lang racket

(provide
  (contract-out
   [target (-> pict?)]
   [range (-> pict?)]))

(require pict
         pict/color
         pict/shadow
         frosthaven-manager/qi
         frosthaven-manager/aoe-images)

(define-flow highlight
  (shadow 10 0 0 #:shadow-color "white"))

(define (target)
  (~> (20 40)
      (>< (circle #:border-width 5))
      cc-superimpose
      highlight))

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
        (inset 0 0 -10 0)
        highlight)))

(module+ main
  (require (only-in racket/gui))
  (for ([p (list target
                 range)])
    (show-pict (p))))
