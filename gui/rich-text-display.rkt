#lang racket

(provide
 (all-from-out 'model)
 (contract-out
  [rich-text-display
   (->* ((maybe-obs/c (listof (or/c string?
                                    pict:pict?
                                    pict/alt-text?
                                    newline?))))
        (#:font (maybe-obs/c (is-a?/c font%))
         #:min-size (maybe-obs/c size/c)
         #:stretch (maybe-obs/c stretch/c)
         #:margin (maybe-obs/c margin/c)
         #:inset (maybe-obs/c margin/c)
         #:style (listof (one-of/c 'no-border 'control-border 'combo
                                   'no-hscroll 'no-vscroll
                                   'hide-hscroll 'hide-vscroll
                                   'auto-vscroll 'auto-hscroll
                                   'resize-corner 'deleted 'transparent)))
        (is-a?/c view<%>))]))

(require (except-in racket/gui newline)
         racket/gui/easy
         racket/gui/easy/contract
         racket/gui/easy/observable
         pict/snip
         (prefix-in pict: pict)
         frosthaven-manager/gui/mixins)

(module model racket
  (provide
   (contract-out
    [struct pict/alt-text ([p pict?]
                           [alt-text string?])]
    [newline? (-> any/c boolean?)]
    [newline newline?]
    [scale-icon (-> pict? pict?)]
    [model-equal? (-> (listof (or/c string?
                                    pict?
                                    pict/alt-text?
                                    newline?))
                      (listof (or/c string?
                                    pict?
                                    pict/alt-text?
                                    newline?))
                      any/c)]))

  (require racket/snip
           racket/draw
           pict)

  (define newline
    (let ([s (make-object string-snip% "\n")])
      (begin0 s
        (send s set-flags (cons 'hard-newline (send s get-flags))))))

  (define (newline? x)
    (eq? x newline))

  (struct pict/alt-text [p alt-text])

  (define icon-sizer (text "MM\nMM"))
  (define (scale-icon p)
    (scale-to-fit p icon-sizer))

  (define (model-equal? xs ys)
    (and (= (length xs) (length ys))
         (andmap (match-lambda**
                   [{(? string? x) (? string? y)} (string=? x y)]
                   [{(? newline?) (? newline?)} #t]
                   [{(? pict? p) (? pict? q)} (pict-equal? p q)]
                   [{(pict/alt-text p t) (pict/alt-text q u)} (and (string=? t u) (pict-equal? p q))]
                   [{_ _} #f])
                 xs ys)))

  (define (pict-equal? p q)
    (equal? (pict->recorded-datum p)
            (pict->recorded-datum q)))

  (define (pict->recorded-datum p)
    (let ([dc (new record-dc%)])
      (draw-pict p dc 0 0)
      (send dc get-recorded-datum))))

(require 'model)

(define (peek v)
  (if (obs? v)
    (obs-peek v)
    v))

(define (font->delta f)
  (define d (new style-delta%))
  (define size-in-pixels? (send f get-size-in-pixels))
  (define size (send f get-size))
  (define family (send f get-family))
  (define face (send f get-face))
  (define style (send f get-style))
  (define weight (send f get-weight))
  (define underline (send f get-underlined))
  (define smoothing (send f get-smoothing))
  (send* d
         (set-delta 'change-size-in-pixels size-in-pixels?)
         (set-delta 'change-size (exact-truncate size))
         (set-delta 'change-family family)
         (set-face face)
         (set-delta 'change-style style)
         (set-delta 'change-weight weight)
         (set-delta 'change-underline underline)
         (set-delta 'change-smoothing smoothing))
  d)

(define (draw editor content font)
  (define δs (list (font->delta font)))
  (send editor begin-edit-sequence)
  (for ([c content])
    (match c
      [(? string? c) (insert-string-and-apply-styles editor c δs)]
      [(? newline?) (insert-newline editor)]
      [(? pict:pict? p) (insert-pict editor p)]
      [(pict/alt-text p alt-text) (insert-pict/alt-text editor p alt-text)]))
  (send editor end-edit-sequence))

(define (insert-string-and-apply-styles editor content styles)
  (define start (send editor last-position))
  (send editor insert (make-object string-snip% content))
  (define end (send editor last-position))
  (for ([style (in-list styles)])
    (send editor change-style style start end #f)))

;; pixels per scroll step, can be set 1 for smooth scrolling
(define ppss 10.0)

(define pict-snip-v2%
  (class pict-snip%
    (init)
    (super-new)

    (define/override (get-num-scroll-steps)
      (define height (pict:pict-height (send this get-pict)))
      (exact-ceiling (/ height ppss)))

    (define/override (get-scroll-step-offset offset)
      (exact-floor (* offset ppss)))

    (define/override (find-scroll-step y)
      (exact-floor (/ y ppss)))))

(define (insert-pict editor p)
  (send editor insert (make-object pict-snip-v2% p)))

(define (insert-pict/alt-text editor p alt-text)
  (send editor insert (make-object pict-snip/alt-text% alt-text p)))

(define (insert-newline editor)
  (define s (make-object string-snip% "\n"))
  (send s set-flags (cons 'hard-newline (send s get-flags)))
  (send editor insert s))

(define pict-snip/alt-text%
  (class pict-snip-v2% (super-new)
    [init-field alt-text]
    (define/override (copy)
      (make-object string-snip% alt-text))))

(define rich-text-display%
  (class* object% (view<%>)
    (init-field @content @font @min-size @margin @inset @stretch style)
    (super-new)

    (define/public (dependencies)
      (filter obs? (list @content @font @min-size @margin @inset @stretch)))

    (define/public (create parent)
      (match-define (list min-w min-h) (peek @min-size))
      (match-define (list w-s? h-s?) (peek @stretch))
      (match-define (list h-m v-m) (peek @margin))
      (match-define (list h-i v-i) (peek @inset))
      (define canvas
        (new (context-mixin editor-canvas%)
             [parent parent]
             [style style]
             [horizontal-inset h-i]
             [vertical-inset v-i]
             [horiz-margin h-m]
             [vert-margin v-m]
             [min-width min-w]
             [min-height min-h]
             [stretchable-width w-s?]
             [stretchable-height h-s?]))
      (define editor (new (hide-caret/selection text%)))
      ((current-text-keymap-initializer)
       (send editor get-keymap))
      (send canvas set-editor editor)
      (send* editor
             (auto-wrap #t)
             (hide-caret #t))
      (draw editor (peek @content) (peek @font))
      (send* editor
             (scroll-to-position 0)
             (lock #t))
      (send canvas set-context 'content (peek @content))
      canvas)

    (define (redraw v content font)
      (define editor (send v get-editor))
      (send editor lock #f)
      (send editor erase)
      (draw editor content font)
      (send editor scroll-to-position 0)
      (send editor lock #t))

    (define/public (update v what val)
      (case/dep what
        [@content
         (unless (model-equal? val (send v get-context 'content))
           (redraw v val (peek @font))
           (send v set-context 'content val))]
        [@font (redraw v (peek @content) val)]
        [@min-size
          (match-define (list min-w min-h) val)
          (send* v
                 (min-width min-w)
                 (min-height min-h))]
        [@stretch
          (match-define (list w-s? h-s?) val)
          (send* v
                 (stretchable-width w-s?)
                 (stretchable-height h-s?))]
        [@margin
          (match-define (list h-m v-m) val)
          (send* v
                 (horiz-margin h-m)
                 (vert-margin v-m))]
        [@inset
          (match-define (list h-i v-i) val)
          (send* v
                 (horizontal-inset h-i)
                 (vertical-inset v-i))]))

    (define/public (destroy v)
      (void))))

(define (rich-text-display @content
                           #:font [@font normal-control-font]
                           #:min-size [@min-size '(#f #f)]
                           #:stretch [@stretch '(#t #t)]
                           #:margin [@margin '(0 0)]
                           #:inset [@inset '(5 5)]
                           #:style [style '(no-hscroll)])
  (new rich-text-display%
       [@content @content]
       [@font @font]
       [@min-size @min-size]
       [@stretch @stretch]
       [@margin @margin]
       [@inset @inset]
       [style style]))

(module* main racket
  (require racket/gui/easy
           frosthaven-manager/gui/render
           frosthaven-manager/elements
           (submod ".."))
  ;; no separate eventspace: block main until this window closed
  (render/eventspace
    (window
      #:size '(400 300)
      (text "Hello from a rich text control!")
      (rich-text-display
       (list
        "· Move -1" newline
        "· Attack +1, " (pict/alt-text (element-pics-unfused (fire)) "Consume Fire") ": Poison")))))
