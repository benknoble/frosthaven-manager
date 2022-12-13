#lang racket

(provide
  (contract-out
    [size natural-number/c]
    [struct element-pics ([name string?]
                          [infused pict?]
                          [waning pict?]
                          [unfused pict?])]
    [fire (-> element-pics?)]
    [ice (-> element-pics?)]
    [air (-> element-pics?)]
    [earth (-> element-pics?)]
    [light (-> element-pics?)]
    [dark (-> element-pics?)]
    [elements (-> (listof element-pics?))]))

(require pict
         pict/color
         pict/flash
         (only-in 2htdp/image
                  wedge)
         racket/draw
         plot/pict
         frosthaven-manager/qi)

(struct element-pics [name infused waning unfused] #:transparent)

(define size 50)
(define trimmed-size (- size 5))

(define-flow half (* 1/2))
(define-flow avg (~> (-< + count) /))

(define (base) (disk size))

(define (half-wedge size color)
  (wedge size 180 "solid" color))
(define (wane color)
  (~> (size)
      half
      (-< (half-wedge "black")
          (~> (half-wedge color) (rotate pi)))
      vc-append))

(define (fire-shape path border-color fill-color)
  (translate
   (dc (lambda (dc dx dy)
        (define old-brush (send dc get-brush))
        (define old-pen (send dc get-pen))
        (send* dc
          (set-brush (new brush% [color fill-color]))
          (set-pen (new pen% [color border-color]))
          (draw-path path dx dy)
          (set-brush old-brush)
          (set-pen old-pen)))
      50 50)
   -5 0))

(define (fire-overlay color1 color2 color3 color4)
  (let* ([q-points (list
                     (list (list 25 45) (list 18 42))
                     (list (list 15 30) (list 16 23))
                     (list (list 18 19) (list 22 12))
                     (list (list 20 07) (list 29 11))
                     (list (list 35 27) (list 37 28))
                     (list (list 40 25) (list 41 21))
                     (list (list 39 18) (list 46 27))
                     (list (list 43 35) (list 39 43))
                     ;; (first _) must be same as (first (first q-points))
                     (list (list 25 45) #f))]
         [first-point (first (first q-points))]
         [better-path
           (let ([p (new dc-path%)])
             (match-define (list x0 y0) first-point)
             (begin0
               p
               (send p move-to x0 y0)
               (for ([pc1 (in-list q-points)]
                     [pc2 (in-list (cdr q-points))])
                 (match-define (list (list p1x p1y) (list cx cy)) pc1)
                 (match-define (list (list p2x p2y) _) pc2)
                 ;;
                 (define x21 (avg p1x cx))
                 (define y21 (avg p1y cy))
                 (send p line-to x21 y21)
                 ;;
                 (define x22 (avg cx p2x))
                 (define y22 (avg cy p2y))
                 (define xm1 (avg x21 cx))
                 (define ym1 (avg y21 cy))
                 (define xm2 (avg cx x22))
                 (define ym2 (avg cy y22))
                 (send p curve-to xm1 ym1 xm2 ym2 x22 y22)
                 ;;
                 (send p line-to p2x p2y))))]
         [fire-shape (flow (fire-shape better-path "black" _))])
    (~> (color1 color2 color3 color4)
        (>< fire-shape)
        (== _
            (~> (scale 0.7 0.7) (translate 0 5))
            (~> (scale 0.5 0.5) (translate 0 9))
            (~> (scale 0.3 0.2) (translate 0 14)))
        cc-superimpose)))

(define (fire)
  (define colored-fire-overlay (fire-overlay "red" "orange" "yellow" "white"))
  (define bw-fire-overlay (fire-overlay "white" "black" "white" "black"))
  (define infused-fire (cc-superimpose (red (base)) colored-fire-overlay))
  (define waning-fire (cc-superimpose (wane "red") colored-fire-overlay))
  (define unfused-fire (cc-superimpose (base) bw-fire-overlay))
  (element-pics "Fire" infused-fire waning-fire unfused-fire))

(define (ice-overlay)
  (let* ([bar (~> ((filled-rectangle 2 trimmed-size)) white (inset 5 0))]
         [branch (white (filled-rectangle 2 5))]
         [fractal
           (~> (bar)
               (pin-over 1 5 (rotate branch (/ pi 3)))
               (pin-over 6 5 (rotate branch (/ pi -3)))
               (pin-over 1 (- size 5 10) (rotate branch (* pi 2/3)))
               (pin-over 6 (- size 5 10) (rotate branch (* pi -2/3))))])
    (~> (fractal)
        (-< _ (rotate (half pi)) (rotate (/ pi 4)) (rotate (/ pi -4)))
        cc-superimpose)))
(define (ice)
  (define overlay (ice-overlay))
  (define infused-ice (cc-superimpose (cyan (base)) overlay))
  (define waning-ice (cc-superimpose (wane "cyan") overlay))
  (define unfused-ice (cc-superimpose (base) overlay))
  (element-pics "Ice" infused-ice waning-ice unfused-ice))

(define (air-overlay)
  (let* ([spiral-plot
           (parameterize ([plot-x-ticks no-ticks]
                          [plot-y-ticks no-ticks]
                          [plot-x-label #f]
                          [plot-y-label #f]
                          [line-width 3]
                          [plot-width (half size)]
                          [plot-height (half size)]
                          [plot-background-alpha 0]
                          [plot-foreground-alpha 0])
             ;; https://en.wikipedia.org/wiki/Archimedean_spiral
             (plot (polar (λ (θ) (* -3 θ))
                          #:color "white")))]
         [spiral (~> (spiral-plot)
                     (scale 1/2 -1/2)
                     (rotate (* pi 1/2)))]
         [bar+spiral (~> (size) half (- 3)
                         (filled-rounded-rectangle 0.5) white
                         (translate 3 0)
                         (hb-append -2 _ spiral))]
         [middle (~> (size) (* 2/3) (- 5)
                     (filled-rounded-rectangle 1) white)])
    (~> (bar+spiral)
        (pin-over 3 15 middle)
        (pin-over 0 30 (scale bar+spiral 1 -1))
        (refocus middle)
        (translate -1 0)
        (hc-append (cloud 15 (* 2/3 size) "white")))))
(define (air)
  (define overlay (air-overlay))
  (define infused-air (cc-superimpose (colorize (base) "light gray") overlay))
  (define waning-air (cc-superimpose (wane "light gray") overlay))
  (define unfused-air (cc-superimpose (base) overlay))
  (element-pics "Air" infused-air waning-air unfused-air))

(define-flow right-isoceles-hypotenuse->leg
  (/ (sqrt 2)))
(define-flow size->dx
  (~> right-isoceles-hypotenuse->leg -))

(define (earth-overlay)
  (let* ([stem (white (filled-rectangle 2 trimmed-size))]
         [large-size (- (half trimmed-size) 3)]
         [large-branch (white (filled-rectangle 2 large-size))]
         [med-size (/ trimmed-size 3)]
         [med-branch (white (filled-rectangle 2 med-size))]
         [small-size (- (/ trimmed-size 5) 3)]
         [small-branch (white (filled-rectangle 2 small-size))])
    (~> (stem)
        (pin-over (size->dx large-size)
                  (half trimmed-size)
                  (rotate large-branch (* pi 1/4)))
        (pin-over (size->dx med-size)
                  (/ trimmed-size 3)
                  (rotate med-branch (* pi 1/4)))
        (pin-over (size->dx small-size)
                  (/ trimmed-size 8)
                  (rotate small-branch (* pi 1/4)))
        (pin-line stem (flow (~> cb-find (== _ (- 2))))
                  stem ct-find
                  #:color "white"
                  #:start-angle (* pi 7/8)
                  #:end-angle (/ pi 3)
                  #:start-pull 3/4
                  #:end-pull 1/4)
        (-< _ (scale -1 1))
        hc-append
        (refocus stem)
        (rotate (* pi -1/4)))))
(define (earth)
  (define overlay (earth-overlay))
  (define infused-earth (cc-superimpose (colorize (base) "dark green") overlay))
  (define waning-earth (cc-superimpose (wane "dark green") overlay))
  (define unfused-earth (cc-superimpose (base) overlay))
  (element-pics "Earth" infused-earth waning-earth unfused-earth))

(define (light-overlay)
  (cc-superimpose (white (outline-flash trimmed-size trimmed-size 8 .55))
                  (white (filled-flash (- size 25) (- size 25) 8 .55))))
(define (light)
  (define overlay (light-overlay))
  (define infused-light (cc-superimpose (colorize (base) "gold") overlay))
  (define waning-light (cc-superimpose (wane "gold") overlay))
  (define unfused-light (cc-superimpose (base) overlay))
  (element-pics "Light" infused-light waning-light unfused-light))

(define (dark-disks color)
  (flow (~> (pin-over (- (half size) 6) (/ size 4)
                      (disk (half size) #:color "white" #:border-color color #:border-width 1))
            (pin-over 6 (/ size 4)
                      (disk (half size) #:color color #:border-color "white" #:border-width 1)))))
(define (dark)
  (define infused-dark (~> ((base)) (colorize "purple") (dark-disks "purple")))
  (define waning-dark (~> ("purple") wane (dark-disks "purple")))
  (define unfused-dark (~> ((base)) (dark-disks "black")))
  (element-pics "Dark" infused-dark waning-dark unfused-dark))

(define (elements) (list (fire) (ice) (air) (earth) (light) (dark)))
