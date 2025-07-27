#lang racket

(provide
  (contract-out
    [hex-size (parameter/c natural-number/c)]
    [r (-> (and/c positive? number?))]
    [S (-> pict?)]
    [X (-> pict?)]
    [O (-> pict?)]
    [M (-> pict?)]
    [border-size (-> natural-number/c natural-number/c (and/c positive? number?))]
    [spec-sym? flat-contract?]
    [spec? flat-contract?]
    [spec->shape (-> spec? pict?)]
    [syntaxes-can-be-spec? (-> any/c boolean?)]
    [syntaxes->spec (-> (and/c (listof syntax?) syntaxes-can-be-spec?) spec?)]
    [string->spec (-> string? spec?)]))

(require frosthaven-manager/curlique
         pict
         racket/draw)

(module+ test (require rackunit))

(define (custom-hex s)
  (define h (* (sqrt 3) s))
  (define r (* 1/2 h))
  (define extra-dy (* 1/2 s))

  (define p (new dc-path%))
  (send* p
         (move-to 0 0)
         (line-to 0 s)
         (line-to r (* 3/2 s))
         (line-to (* 2 r) s)
         (line-to (* 2 r) 0)
         (line-to r (* -1/2 s))
         (close))

  (dc (λ (dc dx dy)
        (define old-pen (send dc get-pen))
        (send* dc
               (set-pen "black" 1 'solid)
               (draw-path p dx (+ dy (* 1/2 extra-dy)))
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
  (~> ((S))
      (-< (~> pict-height (* max-row))
          (~> pict-width (* max-col)))
      max (* 3/2)))

(struct line [number dedent? columns] #:prefab)
;; shape: spec-sym?
(struct column [shape number] #:prefab)

;; spec? -> spec?
(define (fill-in-spec s)
  (let loop ([s s]
             [result null]
             [last-line (match s
                          ['() 0]
                          [(cons [line number _ _] _) (sub1 number)])])
    (match s
      [(cons [line (and line-number (== (add1 last-line))) dedent? columns] s)
       (loop s
             (cons [line line-number dedent? (fill-in-columns columns)] result)
             line-number)]
      [(cons a-line s)
       (loop (cons a-line s)
             (cons [line (add1 last-line) #f '()] result)
             (add1 last-line))]
      ['() (reverse result)])))

(module+ test
  (check-equal? (fill-in-spec (string->spec "x m x"))
                (list (line 1 #f (list (column 'x 0)
                                       (column 'm 1)
                                       (column 'x 2)))))
  (check-equal? (fill-in-spec (string->spec " x m x"))
                (list (line 1 #f (list (column 'x 0)
                                       (column 'm 1)
                                       (column 'x 2)))))
  (check-equal? (fill-in-spec (string->spec " x x\nx x x\n x x"))
                (list (line 1 #t (list (column 'x 0)
                                       (column 'x 1)))
                      (line 2 #f (list (column 'x 0)
                                       (column 'x 1)
                                       (column 'x 2)))
                      (line 3 #t (list (column 'x 0)
                                       (column 'x 1)))))
  (check-equal? (fill-in-spec (string->spec " x x\nx   x\n x x"))
                (list (line 1 #t (list (column 'x 0)
                                       (column 'x 1)))
                      (line 2 #f (list (column 'x 0)
                                       (column 'g 2)
                                       (column 'x 2)))
                      (line 3 #t (list (column 'x 0)
                                       (column 'x 1)))))
  (check-equal? (fill-in-spec (string->spec #<<EOF

   x
  m x
EOF
                                           ))
                (list (line 2 #t (list (column 'g 1)
                                       (column 'x 1)))
                      (line 3 #f (list (column 'g 1)
                                       (column 'm 1)
                                       (column 'x 2))))))

;; (listof column?) -> (listof column?)
(define (fill-in-columns cs)
  (let loop ([cs cs]
             [result null]
             [last-column -1])
    (match cs
      [(cons (and c [column _ (and column-number (== (add1 last-column)))]) cs)
       (loop cs (cons c result) column-number)]
      [(cons (and c [column _ column-number]) cs)
       (loop (cons c cs)
             ;; we don't actually end up using this number when building the
             ;; shape!
             (cons [column 'g column-number] result)
             (add1 last-column))]
      ['() (reverse result)])))

(define spec-sym? (or/c 's 'x 'o 'm 'g))

(define (sym->shape s)
  (case s
    [(s) (S)]
    [(x) (X)]
    [(o) (O)]
    [(m) (M)]
    [(g) (ghost (S))]))

;; (listof spec-sym?) -> pict?
(define (row->shape r)
  (match r
    ['() (ghost (S))]
    [(list [column sym _] ...) (apply hc-append (map sym->shape sym))]))

;; spec? -> pict?
(define (rows->shape rs)
  (define max-row (length rs))
  (define max-col
    (apply max (map (match-lambda [[line _ _ row-spec] (length row-spec)]) rs)))
  (define frame-length (border-size max-row max-col))
  (cc-superimpose
    (rectangle frame-length frame-length)
    (for/fold ([p (blank)])
              ([row (in-list rs)])
      (match-define [line _ dedent? row-spec] row)
      (define dx (if dedent? 0 (- (r))))
      (vl-append p (translate (row->shape row-spec) dx 0)))))

(define spec->shape {~> fill-in-spec rows->shape})

(define (syntaxes->spec stxs)
  (define groups (group-by syntax-line stxs))
  (define line-with-least-column
    (syntax-line (argmin syntax-column stxs)))
  (define dedent? (if (even? line-with-least-column) odd? even?))
  (for/list ([group (in-list groups)])
    (define line-number (syntax-line (first group)))
    (line
     line-number
     (dedent? line-number)
     (for/list ([stx (in-list group)])
       (column (syntax-e stx)
               (exact-floor (/ (syntax-column stx) 2)))))))

;; pre-condition: ((listof syntax?) stxs)
(define (syntaxes-can-be-spec? stxs)
  (for/and ([stx (in-list stxs)])
    (and (syntax-line stx)
         (spec-sym? (syntax-e stx)))))

(define spec?
  (listof (struct/c line exact-positive-integer? boolean? (listof (struct/c column spec-sym? natural-number/c)))))

(define string->spec
  {~>> open-input-string
       (ε port-count-lines!)
       (port->list {(read-syntax #f _)})
       syntaxes->spec})
