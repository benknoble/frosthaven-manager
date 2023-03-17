#lang racket

(provide
  (contract-out
    [hex-size (parameter/c natural-number/c)]
    [r (-> (and/c positive? number?))]
    [S (-> pict?)]
    [X (-> pict?)]
    [O (-> pict?)]
    [M (-> pict?)]
    [border-size (-> natural-number/c natural-number/c (and/c positive? number?))]))

(require pict
         racket/draw)

(define (custom-hex s)
  (define h (* (sqrt 3) s))
  (define r (* 1/2 h))
  (define extra-dy (* 1/2 s))

  (define path
    (let ([p (new dc-path%)])
      (begin0 p
        (send* p
               (move-to 0 0)
               (line-to 0 s)
               (line-to r (* 3/2 s))
               (line-to (* 2 r) s)
               (line-to (* 2 r) 0)
               (line-to r (* -1/2 s))
               (close)))))

  (dc (λ (dc dx dy)
        (define old-pen (send dc get-pen))
        (send* dc
               (set-pen "black" 1 'solid)
               (draw-path path dx (+ dy (* 1/2 extra-dy)))
               (set-pen old-pen)))
      h (+ s extra-dy)
      (* 1/2 extra-dy) (* 1/2 extra-dy)))

(define hex-size (make-parameter 30))
(define (r)
  (* 1/2 (sqrt 3) (hex-size)))
(define (S)
  (custom-hex (hex-size)))
(define (X)
  (cc-superimpose (colorize (S) "red")
                  (colorize (text "X" null (* 2/3 (hex-size))) "white")))
(define (O)
  (cc-superimpose (colorize (S) "cyan")
                  (colorize (custom-hex (* 2/3 (hex-size))) "blue")))
(define (M)
  (colorize (S) "gray"))

(define (border-size max-row max-col)
  (* 3/2 (max (* (pict-height (S)) max-row)
              (* (pict-width (S)) max-col))))

;; (define top (hc-append (X) (X)))
;; (define middle (translate (hc-append (X) (X) (X) (M) (O)) (- (r)) 0))
;; (define bottom (hc-append (X) (X)))
;; (define extra (translate (hc-append (X) (ghost (S)) (X)) (- (r)) 0))
;; (require racket/gui)
;; (show-pict
;;   (explain-child (cc-superimpose (rectangle (border-size 4 5) (border-size 4 5))
;;                                  (vl-append top middle bottom extra))
;;                  top middle bottom extra
;;                  #:scale 1))
;; (show-pict (cc-superimpose (rectangle (border-size 1 1) (border-size 1 1))
;;                            (X)))
