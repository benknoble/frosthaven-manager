#lang racket

(provide
  (contract-out
    [copy-font
      (->* ((is-a?/c font%))
           (#:size (real-in 0.0 1024.0)
            #:face (or/c string? #f)
            #:family (or/c 'default 'decorative 'roman 'script 'swiss 'modern 'symbol 'system)
            #:style (or/c 'normal 'italic 'slant)
            #:weight font-weight/c
            #:underlined? any/c
            #:smoothing (or/c 'default 'partly-smoothed 'smoothed 'unsmoothed)
            #:size-in-pixels? any/c
            #:hinting (or/c 'aligned 'unaligned)
            #:feature-settings font-feature-settings/c
            #:font-list (or/c (is-a?/c font-list%) #f))
           (is-a?/c font%))]
    [big-control-font (is-a?/c font%)]))

(require racket/gui)

(define (copy-font f
                   #:font-list [font-list (current-font-list)]
                   #:face [face (send f get-face)]
                   #:family [family (send f get-family)]
                   #:size-in-pixels? [size-in-pixels? (send f get-size-in-pixels)]
                   #:size [size (send f get-size size-in-pixels?)]
                   #:style [style (send f get-style)]
                   #:weight [weight (send f get-weight)]
                   #:underlined? [underlined? (send f get-underlined)]
                   #:smoothing [smoothing (send f get-smoothing)]
                   #:hinting [hinting (send f get-hinting)]
                   #:feature-settings [feature-settings (send f get-feature-settings)])
  (make-font #:font-list font-list
             #:face face
             #:family family
             #:size-in-pixels? size-in-pixels?
             #:size size
             #:style style
             #:weight weight
             #:underlined? underlined?
             #:smoothing smoothing
             #:hinting hinting
             #:feature-settings feature-settings))

(define big-control-font
  (copy-font normal-control-font
             #:font-list the-font-list
             #:size (+ 10 (send normal-control-font get-size))
             #:style 'italic))
