#lang racket

(provide
  (contract-out
    [size natural-number/c]
    [struct element-pics ([name string?]
                          [infused pict?]
                          [waning pict?]
                          [unfused pict?]
                          [consume pict?])]
    [fire (-> element-pics?)]
    [ice (-> element-pics?)]
    [air (-> element-pics?)]
    [earth (-> element-pics?)]
    [light (-> element-pics?)]
    [dark (-> element-pics?)]
    [elements (-> (listof element-pics?))]
    [wild (-> element-pics?)]))

(require frosthaven-manager/curlique
         pict
         pict/color
         pict/flash
         racket/draw
         (only-in 2htdp/image wedge))

(struct element-pics [name infused waning unfused consume] #:transparent)

(define size 50)
(define trimmed-size (- size 5))

(define-flow half (* 1/2))
(define avg {~> (-< + count) /})

(define (base) (disk size))

(define (half-wedge size color)
  (wedge size 180 "solid" color))
(define (wane color)
  (~> (size)
      half
      (-< (half-wedge "black")
          (~> (half-wedge color) (rotate pi)))
      vc-append))

(define (consume-icon)
  (~> (size) half
      (disk #:color "tomato" #:border-color "white" #:border-width 1)
      (cc-superimpose (colorize (text "x") "white"))))

(define (make-consume infused)
  (rb-superimpose infused (consume-icon)))

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
         [fire-shape {(fire-shape better-path "black" _)}])
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
  (element-pics "Fire" infused-fire waning-fire unfused-fire (make-consume infused-fire)))

(define (ice-overlay)
  (define bar (~> ((filled-rectangle 2 trimmed-size)) white (inset 5 0)))
  (define branch (white (filled-rectangle 2 5)))
  (define fractal
    (~> (bar)
        (pin-over 1 5 (rotate branch (/ pi 3)))
        (pin-over 6 5 (rotate branch (/ pi -3)))
        (pin-over 1 (- size 5 10) (rotate branch (* pi 2/3)))
        (pin-over 6 (- size 5 10) (rotate branch (* pi -2/3)))))
  (~> (fractal) (-< _ (rotate (half pi)) (rotate (/ pi 4)) (rotate (/ pi -4))) cc-superimpose))
(define (ice)
  (define overlay (ice-overlay))
  (define infused-ice (cc-superimpose (cyan (base)) overlay))
  (define waning-ice (cc-superimpose (wane "cyan") overlay))
  (define unfused-ice (cc-superimpose (base) overlay))
  (element-pics "Ice" infused-ice waning-ice unfused-ice (make-consume infused-ice)))

(define (air-overlay)
  (let* ([n-samples 500]
         ;; https://en.wikipedia.org/wiki/Archimedean_spiral
         [spiral {(* -3)}]
         [θs (range 0 (* 2 pi) (/ (* 2 pi) n-samples))]
         [rs (map spiral θs)]
         [polar->cartesian (λ (r θ) (~> (θ) (-< cos sin) (>< (* r))))]
         [points (map {~> polar->cartesian collect} rs θs)]
         [path (let ([p (new dc-path%)])
                 (match-define (cons (list x0 y0) pts) points)
                 (define xm (apply min (map first pts)))
                 (define ym (* 2 (apply min (map second pts))))
                 (begin0 p
                   (send p move-to (- x0 xm) (- y0 ym))
                   (for ([pt (in-list pts)])
                     (match-define (list x y) pt)
                     (send p line-to (- x xm) (- y ym)))))]
         [spiral-plot
           (scale
             (dc
               (lambda (dc dx dy)
                 (define old-brush (send dc get-brush))
                 (define old-pen (send dc get-pen))
                 (send* dc
                   (set-brush (new brush% [style 'transparent]))
                   (set-pen (new pen% [color "white"] [width 4]))
                   (draw-path path dx dy)
                   (set-brush old-brush)
                   (set-pen old-pen)))
               30 30)
             30/25 -30/25)]
         [spiral (~> (spiral-plot)
                     (scale 1/3 -1/2)
                     (rotate (* pi 1/2)))]
         [bar+spiral (~> (size) half (- 5)
                         (filled-rounded-rectangle 0.5) white
                         (translate 4 0.5)
                         (hb-append -3 _ spiral))]
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
  (element-pics "Air" infused-air waning-air unfused-air (make-consume infused-air)))

(define-flow right-isoceles-hypotenuse->leg
  (/ (sqrt 2)))
(define size->dx
  {~> right-isoceles-hypotenuse->leg -})

(define (earth-overlay)
  (define stem (white (filled-rectangle 2 trimmed-size)))
  (define large-size (- (half trimmed-size) 3))
  (define large-branch (white (filled-rectangle 2 large-size)))
  (define med-size (/ trimmed-size 3))
  (define med-branch (white (filled-rectangle 2 med-size)))
  (define small-size (- (/ trimmed-size 5) 3))
  (define small-branch (white (filled-rectangle 2 small-size)))
  (~> (stem)
      (pin-over (size->dx large-size) (half trimmed-size) (rotate large-branch (* pi 1/4)))
      (pin-over (size->dx med-size) (/ trimmed-size 3) (rotate med-branch (* pi 1/4)))
      (pin-over (size->dx small-size) (/ trimmed-size 8) (rotate small-branch (* pi 1/4)))
      (pin-line stem
                {~> cb-find (== _ (- 2))}
                stem
                ct-find
                #:color "white"
                #:start-angle (* pi 7/8)
                #:end-angle (/ pi 3)
                #:start-pull 3/4
                #:end-pull 1/4)
      (-< _ (scale -1 1))
      hc-append
      (refocus stem)
      (rotate (* pi -1/4))))
(define (earth)
  (define overlay (earth-overlay))
  (define infused-earth (cc-superimpose (colorize (base) "dark green") overlay))
  (define waning-earth (cc-superimpose (wane "dark green") overlay))
  (define unfused-earth (cc-superimpose (base) overlay))
  (element-pics "Earth" infused-earth waning-earth unfused-earth (make-consume infused-earth)))

(define (light-overlay)
  (cc-superimpose (white (outline-flash trimmed-size trimmed-size 8 .55))
                  (white (filled-flash (- size 25) (- size 25) 8 .55))))
(define (light)
  (define overlay (light-overlay))
  (define infused-light (cc-superimpose (colorize (base) "gold") overlay))
  (define waning-light (cc-superimpose (wane "gold") overlay))
  (define unfused-light (cc-superimpose (base) overlay))
  (element-pics "Light" infused-light waning-light unfused-light (make-consume infused-light)))

(define (dark-disks color)
  {~> (pin-over (- (half size) 6) (/ size 4)
                (disk (half size) #:color "white" #:border-color color #:border-width 1))
      (pin-over 6 (/ size 4)
                (disk (half size) #:color color #:border-color "white" #:border-width 1))})
(define (dark)
  (define infused-dark (~> ((base)) (colorize "purple") (esc (dark-disks "purple"))))
  (define waning-dark (~> ("purple") wane (esc (dark-disks "purple"))))
  (define unfused-dark (~> ((base)) (esc (dark-disks "black"))))
  (element-pics "Dark" infused-dark waning-dark unfused-dark (make-consume infused-dark)))

(define (elements) (list (fire) (ice) (air) (earth) (light) (dark)))

(define (wild-base)
  (define (slice color)
    (wedge (* size 1/2) 60 "solid" color))
  (define wild-fire (slice "red"))
  (define wild-ice (slice "cyan"))
  (define wild-air (slice "light gray"))
  (define wild-earth (slice "dark green"))
  (define wild-light (slice "gold"))
  (define wild-dark (slice "purple"))
  ;; TODO: use a dc, draw-arc, etc.
  (~> ((base))
      ghost
      (pin-over (* size 1/2) 0 wild-fire)
      (pin-over 0 (* size 10/24) (rotate wild-ice pi))
      (pin-over (* size 1/8) (* size -17/80) (rotate wild-air (* 1/3 pi)))
      (pin-over (* size 1/4) (* size 10/24) (rotate wild-earth (* -2/3 pi)))
      (pin-over (* size -1/8) (* size -1/80) (rotate wild-light (* 2/3 pi)))
      (pin-over (* size 1/2) (* size 1/5) (rotate wild-dark (* -1/3 pi)))))

(define (wild)
  (define base (wild-base))
  (element-pics "Wild" base base base (make-consume base)))

(module+ main
  (require racket/gui)
  (show-pict
   (apply hc-append
          (for/list ([e (cons (wild) (elements))])
            (apply vc-append
                   (for/list ([f (list element-pics-infused
                                       element-pics-waning
                                       element-pics-unfused
                                       element-pics-consume)])
                     (f e)))))))
