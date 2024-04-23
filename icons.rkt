#lang racket

(provide
  (contract-out
   [target (-> pict?)]
   [range (-> pict?)]
   [push (-> pict?)]
   [pull (-> pict?)]))

(require pict
         pict/color
         pict/shadow
         frosthaven-manager/curlique
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

(define (push)
  (define (outlined-arrowhead size)
    (cc-superimpose (arrowhead size 0)
                    (white (scale (arrowhead size 0) 6/10))))
  (define arrows
    (map {~> outlined-arrowhead (rotate (* 1/2 pi))}
         (list 20 10 5)))

  (~> (arrows) sep
      (vc-append -2 __)
      (scale 1 3/4)
      highlight))

(define (pull)
  (rotate (push) pi))

(module+ main
  (require (only-in racket/gui))
  (for ([p (list target
                 range
                 push
                 pull)])
    (show-pict (p))))
