#lang racket

(provide pict-text-display)

(require frosthaven-manager/observable-operator
         (prefix-in gui: racket/gui)
         racket/gui/easy
         (prefix-in pict: pict)
         (submod frosthaven-manager/gui/rich-text-display model))

(define (pict-text-display @content
                           #:label [@label #f]
                           #:font [@font gui:normal-control-font]
                           #:min-size [@min-size '(#f #f)]
                           #:stretch [@stretch '(#t #t)]
                           #:margin [@margin '(0 0)]
                           #:inset [@inset '(5 5)]
                           #:style [style '()])
  (define @args (obs-combine list (@ @content) (@ @font) (@ @inset)))
  (pict-canvas
   @args
   content->pict
   #:label @label
   #:min-size @min-size
   #:stretch @stretch
   #:margin @margin
   #:style style
   #:mixin (mixin (gui:canvas<%>) (gui:canvas<%>)
             (init [style null])
             (super-new [style (list* 'vscroll 'hscroll style)])
             (define (setup-scrollbars)
               (let* ([p (content->pict (@! @args))]
                      [h (min 1000000 (exact-ceiling (pict:pict-height p)))]
                      [w (min 1000000 (exact-ceiling (pict:pict-width p)))])
                 (send this init-auto-scrollbars w h 0 0)))
             (setup-scrollbars)
             (obs-observe! @args (thunk* (setup-scrollbars)))
             (define/override (on-size window-width window-height)
               (super on-size window-width window-height)
               (define-values (w h) (send this get-client-size))
               (define-values (vw vh) (send this get-virtual-size))
               (send this show-scrollbars (> vw w) (> vh h))))))

(define (content->pict args)
  (match-define (list cs font (list h-i v-i)) args)
  (let loop ([cs cs]
             [accum empty])
    (cond
      [(empty? cs) (pict:inset (apply pict:vl-append 2 (reverse accum))
                               h-i v-i)]
      [else
       (define-values (group rest)
         (splitf-at cs {(not newline?)}))
       (loop
        ;; drop the newline!
        (if (empty? rest) rest (cdr rest))
        (cons (apply pict:hb-append (map {(model->pict font)} group))
              accum))])))

(define (model->pict m font)
  (match m
    [(? string? s) (pict:text s font)]
    [(? newline?) (raise-argument-error 'model->pict "(not/c newline?)" m)]
    [(? pict:pict? p) p]
    [(pict/alt-text p _alt-text) p]))

(module+ main
  (require frosthaven-manager/gui/render
           frosthaven-manager/elements)
  ;; no separate eventspace: block main until this window closed
  (render/eventspace
   (window
    #:size '(400 300)
    (text "Hello from a rich text control!")
    (pict-text-display
     (list
      "· Move -1" newline
      "· Attack +1, " (pict/alt-text (element-pics-unfused (fire)) "Consume Fire") ": Poison" newline
      "· More actions here")))))
