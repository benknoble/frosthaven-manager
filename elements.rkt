#lang racket

(provide
  (contract-out
    [size natural-number/c]
    [struct element-pics ([name string?]
                          [infused pict?]
                          [waning pict?]
                          [unfused pict?])]
    [fire element-pics?]
    [ice element-pics?]
    [air element-pics?]
    [earth element-pics?]
    [light element-pics?]
    [dark element-pics?]
    [elements (listof element-pics?)]))

(require pict
         pict/color
         pict/flash
         (only-in 2htdp/image
                  wedge)
         "qi.rkt")

(struct element-pics [name infused waning unfused] #:transparent)

(define size 50)
(define trimmed-size (- size 5))

(define-flow half (* 1/2))

(define base (disk size))

(define (half-wedge size color)
  (wedge size 180 "solid" color))
(define (wane color)
  (~> (size)
      half
      (-< (half-wedge "black")
          (~> (half-wedge color) (rotate pi)))
      vc-append))

(define fire-overlay (scale-to-fit (text "🔥") base))
(define infused-fire (cc-superimpose (red base) fire-overlay))
(define waning-fire (cc-superimpose (wane "red") fire-overlay))
(define unfused-fire (cc-superimpose base fire-overlay))
(define fire (element-pics "Fire" infused-fire waning-fire unfused-fire))

(define ice-overlay
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
(define infused-ice (cc-superimpose (cyan base) ice-overlay))
(define waning-ice (cc-superimpose (wane "cyan") ice-overlay))
(define unfused-ice (cc-superimpose base ice-overlay))
(define ice (element-pics "Ice" infused-ice waning-ice unfused-ice))

(define air-overlay (scale-to-fit (text "💨") base))
(define infused-air (cc-superimpose (colorize base "light gray") air-overlay))
(define waning-air (cc-superimpose (wane "light gray") air-overlay))
(define unfused-air (cc-superimpose base air-overlay))
(define air (element-pics "Air" infused-air waning-air unfused-air))


(define-flow right-isoceles-hypotenuse->leg
  (/ (sqrt 2)))
(define-flow size->dx
  (~> right-isoceles-hypotenuse->leg -))

(define earth-overlay
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
(define infused-earth (cc-superimpose (colorize base "dark green") earth-overlay))
(define waning-earth (cc-superimpose (wane "dark green") earth-overlay))
(define unfused-earth (cc-superimpose base earth-overlay))
(define earth (element-pics "Earth" infused-earth waning-earth unfused-earth))

(define light-overlay (cc-superimpose (white (outline-flash trimmed-size trimmed-size 8 .55))
                                      (white (filled-flash (- size 25) (- size 25) 8 .55))))
(define infused-light (cc-superimpose (colorize base "gold") light-overlay))
(define waning-light (cc-superimpose (wane "gold") light-overlay))
(define unfused-light (cc-superimpose base light-overlay))
(define light (element-pics "Light" infused-light waning-light unfused-light))

(define infused-dark (pin-over (pin-over (colorize base "purple")
                                         (- (half size) 6) (/ size 4)
                                         (disk (half size) #:color "white" #:border-color "purple" #:border-width 1))
                               6 (/ size 4)
                               (disk (half size) #:color "purple" #:border-color "white" #:border-width 1)))
(define waning-dark (pin-over (pin-over (wane "purple")
                                        (- (half size) 6) (/ size 4)
                                        (disk (half size) #:color "white" #:border-color "purple" #:border-width 1))
                              6 (/ size 4)
                              (disk (half size) #:color "purple" #:border-color "white" #:border-width 1)))
(define unfused-dark (pin-over (pin-over base
                                         (- (half size) 6) (/ size 4)
                                         (disk (half size) #:color "white" #:border-color "black" #:border-width 1))
                               6 (/ size 4)
                               (disk (half size) #:color "black" #:border-color "white" #:border-width 1)))
(define dark (element-pics "Dark" infused-dark waning-dark unfused-dark))

(define elements (list fire ice air earth light dark))
