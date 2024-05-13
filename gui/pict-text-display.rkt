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
  (pict-canvas (obs-combine list (@ @content) (@ @font) (@ @inset))
               content->pict
               #:label @label
               #:min-size @min-size
               #:stretch @stretch
               #:margin @margin
               #:style style))

;; TODO: make auto-vscroll or something similar work…
;; tried: init-auto-scrollbars, but not working

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
