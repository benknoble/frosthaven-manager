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
                  wedge))

(struct element-pics [name infused waning unfused] #:transparent)

(define size 50)

(define base (disk size))

(define (wane color)
  (vc-append (wedge (/ size 2) 180 "solid" "black")
             (rotate (wedge (/ size 2) 180 "solid" color) pi)))

(define fire-overlay (scale-to-fit (text "🔥") base))
(define infused-fire (cc-superimpose (red base) fire-overlay))
(define waning-fire (cc-superimpose (wane "red") fire-overlay))
(define unfused-fire (cc-superimpose base fire-overlay))
(define fire (element-pics "Fire" infused-fire waning-fire unfused-fire))

(define ice-overlay
  (let* ([bar (inset (white (filled-rectangle 2 (- size 5))) 5 0)]
         [branch (white (filled-rectangle 2 5))]
         [fractal (pin-over (pin-over (pin-over (pin-over bar
                                                          1 5
                                                          (rotate branch (/ pi 3)))
                                                6 5
                                                (rotate branch (/ (- pi) 3)))
                                      1 (- size 5 10)
                                      (rotate branch (* 2 (/ pi 3))))
                            6 (- size 5 10)
                            (rotate branch (* (- 2) (/ pi 3))))])
    (cc-superimpose fractal
                    (rotate fractal (/ pi 2))
                    (rotate fractal (/ pi 4))
                    (rotate fractal (/ (- pi) 4)))))
(define infused-ice (cc-superimpose (cyan base) ice-overlay))
(define waning-ice (cc-superimpose (wane "cyan") ice-overlay))
(define unfused-ice (cc-superimpose base ice-overlay))
(define ice (element-pics "Ice" infused-ice waning-ice unfused-ice))

(define air-overlay (scale-to-fit (text "💨") base))
(define infused-air (cc-superimpose (colorize base "light gray") air-overlay))
(define waning-air (cc-superimpose (wane "light gray") air-overlay))
(define unfused-air (cc-superimpose base air-overlay))
(define air (element-pics "Air" infused-air waning-air unfused-air))

(define earth-overlay (scale-to-fit (text "🌿") base))
(define infused-earth (cc-superimpose (colorize base "dark green") earth-overlay))
(define waning-earth (cc-superimpose (wane "dark green") earth-overlay))
(define unfused-earth (cc-superimpose base earth-overlay))
(define earth (element-pics "Earth" infused-earth waning-earth unfused-earth))

(define light-overlay (cc-superimpose (white (outline-flash (- size 5) (- size 5) 8 .55))
                                      (white (filled-flash (- size 25) (- size 25) 8 .55))))
(define infused-light (cc-superimpose (colorize base "gold") light-overlay))
(define waning-light (cc-superimpose (wane "gold") light-overlay))
(define unfused-light (cc-superimpose base light-overlay))
(define light (element-pics "Light" infused-light waning-light unfused-light))

(define infused-dark (pin-over (pin-over (colorize base "purple")
                                         (- (/ size 2) 6) (/ size 4)
                                         (disk (/ size 2) #:color "white" #:border-color "purple" #:border-width 1))
                               6 (/ size 4)
                               (disk (/ size 2) #:color "purple" #:border-color "white" #:border-width 1)))
(define waning-dark (pin-over (pin-over (wane "purple")
                                        (- (/ size 2) 6) (/ size 4)
                                        (disk (/ size 2) #:color "white" #:border-color "purple" #:border-width 1))
                              6 (/ size 4)
                              (disk (/ size 2) #:color "purple" #:border-color "white" #:border-width 1)))
(define unfused-dark (pin-over (pin-over base
                                         (- (/ size 2) 6) (/ size 4)
                                         (disk (/ size 2) #:color "white" #:border-color "black" #:border-width 1))
                               6 (/ size 4)
                               (disk (/ size 2) #:color "black" #:border-color "white" #:border-width 1)))
(define dark (element-pics "Dark" infused-dark waning-dark unfused-dark))

(define elements (list fire ice air earth light dark))
