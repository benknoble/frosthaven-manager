#lang racket

(provide
  (contract-out
   [target (-> pict?)]
   [range (-> pict?)]
   [push (-> pict?)]
   [pull (-> pict?)]
   [move (-> pict?)]
   [jump (-> pict?)]))

(require pict
         pict/color
         pict/shadow
         racket/draw
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

(define boot-scale 3)
(define (scale-point scale)
  {~> (-< car cdr) (>< (* scale)) cons})

(define boot-path
  (let ([p (new dc-path%)])
    (begin0 p
      (send* p
             [move-to 0 0]
             [lines (map (scale-point boot-scale)
                         '((0 . 10)
                           (2 . 10)
                           (3 . 9)
                           (4 . 10)
                           (6 . 10)
                           (8 . 9)
                           (4 . 6)
                           (4 . 0)
                           (0 . 0)
                           (0 . 10)))]
             [close]))))

(define (boot)
  (dc (λ (dc dx dy)
        (define old-brush (send dc get-brush))
        (send* dc
               [set-brush "white" 'solid]
               [draw-path boot-path dx dy]
               [draw-line (+ dx (* boot-scale 6))
                          (+ dy (* boot-scale (- 10 2.5)))
                          (+ dx (* boot-scale 5.7))
                          (+ dy (* boot-scale (- 10 2.1)))]
               [draw-line (+ dx (* boot-scale 16/3))
                          (+ dy (* boot-scale (- 10 3)))
                          (+ dx (* boot-scale 77/15))
                          (+ dy (* boot-scale (- 10 41/15)))]
               [draw-line (+ dx (* boot-scale 4))
                          (+ dy (* boot-scale (- 10 8)))
                          (+ dx (* boot-scale 3))
                          (+ dy (* boot-scale (- 10 8)))]
               [draw-line (+ dx (* boot-scale 4))
                          (+ dy (* boot-scale (- 10 7)))
                          (+ dx (* boot-scale 3))
                          (+ dy (* boot-scale (- 10 7)))]
               [set-brush old-brush]))
      (* boot-scale 8)
      (* boot-scale 10)))

(define (trails)
  (vr-append
   (hline (* boot-scale 8/3) 5)
   (hline (* boot-scale 8/4) 5)
   (hline (* boot-scale 8/3) 5)))

(define (move)
  (~> ((boot))
      (rotate (/ pi -6))
      (ht-append -3 (trails) _)
      highlight))

(define (jump)
  (define squish {(scale 1 1/3)})
  (define-values (left chain right)
    (parameterize ([hex-size 10])
      (define left (squish (S)))
      (define middle (squish (S)))
      (define right (squish (S)))
      (values left
              (hc-append left middle right)
              right)))
  (~> (chain)
      (pin-arrow-line
       5
       _
       left cc-find
       right cc-find
       #:start-angle (* pi 1/6)
       #:end-angle (* pi -1/6)
       #:start-pull 1/2)
      (inset 0 5)
      highlight))

(module+ main
  (require (only-in racket/gui))
  (for ([p (list target
                 range
                 push
                 pull
                 move
                 jump)])
    (show-pict (p))))
