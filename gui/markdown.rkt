#lang racket

(provide
  (contract-out
    [markdown-text
      (->* ((maybe-obs/c (or/c string? path?)))
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

;; Much of the following code is adapted from Alex Harsányi's blog post
;; "Markdown View using the Racket editor%"
;; (https://alex-hhh.github.io/2020/05/markdown-view.html)

;; N.B. Markdown requires 4 spaces or one tab as leading indent for nested lists
;; and other semantically-indented blocks!

(require racket/gui
         racket/gui/easy
         racket/gui/easy/contract
         racket/gui/easy/observable
         net/sendurl
         (only-in pict pict->bitmap hline)
         (only-in markdown
                  parse-markdown
                  current-strict-markdown?)
         (only-in txexpr attr-ref))

(define (peek v)
  (if (obs? v)
    (obs-peek v)
    v))

(define (insert-and-apply-styles editor content styles)
  (define start (send editor last-position))
  (send editor insert (make-object string-snip% content))
  (define end (send editor last-position))
  (for ([style (in-list styles)])
    (send editor change-style style start end #f)))

(define base-s
  (let ([delta (new style-delta%)])
    (send delta set-size-add 4)
    delta))

(define bold-s
  (make-object style-delta% 'change-bold))
(define italic-s
  (make-object style-delta% 'change-style 'italic))
(define code-s
  (let ([delta (new style-delta%)])
    (send delta set-family 'modern)
    delta))
(define bq-s
  (make-object style-delta% 'change-style 'slant))

(define (make-header-style font-increase)
  (define delta (make-object style-delta% 'change-bigger font-increase))
  (send delta set-alignment-on 'base)
  (send delta set-underlined-on #t)
  delta)
(match-define
  (list h1-s h2-s h3-s h4-s h5-s h6-s)
  (map make-header-style '(7 6 5 4 3 2)))

(define hyper-s
  (let ([delta (new style-delta%)])
    (send delta set-delta-foreground (make-object color%  68 119 170))
    (send delta set-underlined-on #t)
    delta))

(define hyper-clicking-s
  (let ([delta (new style-delta%)])
    (send delta set-delta-background (make-object color%  68 119 170))
    (send delta set-underlined-on #t)
    delta))

(define hyper-clicked-s
  (let ([delta (new style-delta%)])
    (send delta set-delta-foreground (make-object color%  119 68 170))
    (send delta set-underlined-on #t)
    delta))

(define (draw-markdown editor content)
  (define md
    (parameterize ([current-strict-markdown? #f])
      (parse-markdown content)))
  (send editor begin-edit-sequence)
  (insert-md-items editor md (list base-s))
  (send editor end-edit-sequence))

(define paragraph-indent (make-parameter 0))
(define list-nesting (make-parameter 0))
(define list-type (make-parameter #f))
(define list-number (make-parameter #f))
(define (insert-md-items editor items styles #:paragraph? [paragraph? #f])
  (when (and paragraph? (not (zero? (send editor last-position))))
    (insert-newline editor)
    (insert-newline editor))
  (define paragraph (send editor last-paragraph))
  (for ([item (in-list items)])
    (insert-md-item editor item styles))
  (let ([pi (paragraph-indent)])
    (when (and paragraph? (positive? pi))
      (send editor set-paragraph-margins paragraph pi pi pi))))

(define paragraph-like '(p h1 h2 h3 h4 h5 h6))
(define (insert-md-item editor item styles)
  (match item
    [(? string? s) (insert-and-apply-styles editor s styles)]
    [(list* '!HTML-COMMENT _) (void)]
    [(list* 'a attrs body)
     (define target (attr-ref attrs 'href #f))
     (insert-hyperlink editor body styles target)]
    [(list* 'blockquote _ body)
     (parameterize ([paragraph-indent (+ 20 (paragraph-indent))])
       (insert-md-items editor body (cons bq-s styles)))]
    [(list* (and name (or 'ul 'ol)) _ body)
     (parameterize ([list-type name]
                    [list-number 1]
                    [list-nesting (add1 (list-nesting))])
       (insert-md-items editor body styles))]
    [(list* 'li _ body) (insert-list-item editor body styles)]
    [(list* 'hr _) (insert-newline editor)
                   (insert-newline editor)
                   (send editor insert (make-object image-snip% (pict->bitmap (hline 9999 10))))]
    [(list* name _ body)
     (insert-md-items
       editor
       body
       (case name
         [(p) styles]
         [(strong) (cons bold-s styles)]
         [(em) (cons italic-s styles)]
         [(code) (cons code-s styles)]
         [(h1) (cons h1-s styles)]
         [(h2) (cons h2-s styles)]
         [(h3) (cons h3-s styles)]
         [(h4) (cons h4-s styles)]
         [(h5) (cons h5-s styles)]
         [(h6) (cons h6-s styles)]
         [else (raise-arguments-error
                 'insert-md-item
                 "tag not supported"
                 "name" name
                 "item" item)])
       #:paragraph? (member name paragraph-like))]))

(define (insert-newline editor)
  (define s (make-object string-snip% "\n"))
  (send s set-flags (cons 'hard-newline (send s get-flags)))
  (send editor insert s))

(define (insert-hyperlink editor items styles target)
  (define start (send editor last-position))
  (insert-md-items editor items (cons hyper-s styles))
  (define end (send editor last-position))
  (when target
    (send editor set-clickback
          start end
          (λ (editor start end)
            (send-url target)
            (send editor change-style hyper-clicked-s start end #f))
          hyper-clicking-s)))

(define (insert-list-item editor items styles)
  (define marker
    (match (list-type)
      ['ol (format "~a. "
                   (let ([n (list-number)])
                     (list-number (add1 n))
                     n))]
      ['ul "• "]))
  (parameterize ([paragraph-indent (* 20 (list-nesting))])
    (insert-md-items editor (cons marker items) styles #:paragraph? #t)))

(define (hide-caret/selection %)
  ;; not using mixin: after-set-position is a method of text% that is not
  ;; exposed by any interface that text% implements
  (class % (super-new)
    (define/augment (after-set-position)
      (send this hide-caret (= (send this get-start-position)
                               (send this get-end-position)))
      (inner (void) after-set-position))))

(define markdown-text%
  (class* object% (view<%>)
    (init-field @content @min-size @margin @inset @stretch style)
    (super-new)

    (define/public (dependencies)
      (filter obs? (list @content @min-size @margin @inset @stretch)))

    (define/public (create parent)
      (match-define (list min-w min-h) (peek @min-size))
      (match-define (list w-s? h-s?) (peek @stretch))
      (match-define (list h-m v-m) (peek @margin))
      (match-define (list h-i v-i) (peek @inset))
      (define canvas
        (new editor-canvas%
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
      (draw-markdown editor (peek @content))
      (send* editor
             (scroll-to-position 0)
             (lock #t))
      canvas)

    (define/public (update v what val)
      (case/dep what
        [@content
          (define editor (send v get-editor))
          (send editor lock #f)
          (send editor erase)
          (draw-markdown editor val)
          (send editor lock #t)]
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

(define (markdown-text @content
                       #:min-size [@min-size '(#f #f)]
                       #:stretch [@stretch '(#t #t)]
                       #:margin [@margin '(0 0)]
                       #:inset [@inset '(5 5)]
                       #:style [style '(no-hscroll)])
  (new markdown-text%
       [@content @content]
       [@min-size @min-size]
       [@stretch @stretch]
       [@margin @margin]
       [@inset @inset]
       [style style]))

(module* main racket
  (require racket/gui/easy
           frosthaven-manager/gui/render
           (submod ".."))
  ;; no separate eventspace: block main until this window closed
  (render/eventspace
    (window
      #:size '(400 300)
      (markdown-text
        (string-join
          '("This is a text which contains a **bold section**, an *italic section* and a"
            "`monospaced section`."
            ""
            "# Header 1 **some bold text**"
            "## Header 2 *some italic text*"
            "### Header 3 `some monospaced text`"
            "#### Header 4"
            "##### Header 5"
            "###### Header 6"
            ""
            "This is a [link](https://example.com)."
            ""
            "Here is a block quote:"
            ""
            "> this the block quote, and below we have a nested quote"
            ""
            ">> this is a nested quote"
            ""
            "This text is back at toplevel."
            ""
            "Ordered and unordered list demo"
            ""
            "1. first item"
            "1. second item"
            "    * first sub item"
            "        1. first sub-sub item"
            "        1. second sub-sub item"
            "    * second sub item"
            "1. third item"
            ""
            "# Overview"
            ""
            "This document showcases the **Markdown View** and it is available as a"
            "[blog post](https://alex-hhh.github.io/2020/05/markdown-view.html)"
            ""
            "# Implementation details"
            ""
            "1. first step"
            "2. second step"
            "    - substep one"
            "    - substep two"
            "3. third step")
          "\n")))))
