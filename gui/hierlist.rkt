#lang racket

(provide
  (contract-out
    [hierlist
      (->* ((maybe-obs/c hierlist/c))
           (#:min-size (maybe-obs/c size/c)
            #:stretch (maybe-obs/c stretch/c)
            #:margin (maybe-obs/c margin/c)
            #:inset (maybe-obs/c margin/c)
            #:style (listof (one-of/c 'no-border 'control-border 'combo
                                      'no-hscroll 'no-vscroll
                                      'hide-hscroll 'hide-vscroll
                                      'auto-vscroll 'auto-hscroll
                                      'resize-corner 'deleted 'transparent)))
           (is-a?/c view<%>))]))

(require racket/gui
         racket/gui/easy
         racket/gui/easy/contract
         racket/gui/easy/observable
         mrlib/hierlist)

(define hierlist/c
  (or/c
    (list/c 'item-list string?
            (recursive-contract (listof hierlist/c)))
    string?))

(define (peek v)
  (if (obs? v)
    (obs-peek v)
    v))

(define item-mixin
  (mixin (hierarchical-list-item<%>)
    ((interface () set-text))
    (inherit get-editor)
    (super-new)
    (define/public (set-text text)
      (define t (get-editor))
      (send t erase)
      (send t insert text))))

(define hierlist%
  (class* object% (view<%>)
    (init-field @item @min-size @margin @inset @stretch style)
    (super-new)

    (define (add-item v item)
      (match item
        [`(item-list ,title ,(list items ...))
          (define sublist (send v new-list item-mixin))
          (send sublist set-text title)
          (for-each (λ (item) (add-item sublist item)) items)]
        [item
          (define i (send v new-item item-mixin))
          (send i set-text item)]))

    (define/public (dependencies)
      (filter obs? (list @item @min-size @margin @inset @stretch)))

    (define/public (create parent)
      (match-define (list min-w min-h) (peek @min-size))
      (match-define (list w-s? h-s?) (peek @stretch))
      (match-define (list h-m v-m) (peek @margin))
      (match-define (list h-i v-i) (peek @inset))
      (define v (new hierarchical-list%
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
      (add-item v (peek @item))
      v)

    (define/public (update v what val)
      (case/dep what
        [@item
          (for ([item (in-list (send v get-items))])
            (send v delete-item item))
          (add-item v val)]
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

;; TODO: attach data to items (the item-any/c-itself? Or "path" to the item?),
;; generate labels from the items, handle on-select
;; TODO: some kind of "selection" so that we can request the selection be
;; visible?
;; TODO: some kind of "expand all"?
(define (hierlist @item
                  #:min-size [@min-size '(#f #f)]
                  #:stretch [@stretch '(#t #t)]
                  #:margin [@margin '(0 0)]
                  #:inset [@inset '(5 5)]
                  #:style [style '(no-hscroll)])
  (new hierlist%
       [@item @item]
       [@min-size @min-size]
       [@stretch @stretch]
       [@margin @margin]
       [@inset @inset]
       [style style]))

(module* main racket
  (require racket/gui/easy
           (submod ".."))
  (render ;; not setting current renderer
    (window
      (hierlist
        `(item-list
           "A"
           ("B"
            (item-list "C" ("D"))
            (item-list
              "E"
              ((item-list "F" ())
               "G"))))))))
