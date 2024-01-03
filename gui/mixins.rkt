#lang racket

;; cf. https://github.com/Bogdanp/racket-gui-easy/issues/13#issuecomment-1093700596
;; cf. https://github.com/Bogdanp/racket-gui-easy/issues/13#issuecomment-1093153178

(provide make-closing-proc-mixin
         make-on-close-mixin
         define-close!
         hide-caret/selection)

(require (only-in racket/gui/base top-level-window<%>)
         syntax/parse/define)

;; Dialogs need to be closed, but rendering a dialog yields so there's
;; no way to retrieve a dialog's renderer from within itself.  This
;; may be another argument for gui-easy providing a managed
;; `current-renderer'.  In the mean time, we can abuse mixins for this
;; purpose.

;; calls `out` with `close-proc`, which closes the window when invoked
(define (make-closing-proc-mixin out)
  (mixin (top-level-window<%>) (top-level-window<%>) (super-new)
    (out (λ ()
           (when (send this can-close?)
             (send this on-close)
             (send this show #f))))))

;; calls `proc` when the window closes
(define (make-on-close-mixin proc)
  (mixin (top-level-window<%>) (top-level-window<%>) (super-new)
    (define/augment (on-close)
      (proc))))

(define-syntax-parse-rule (define-close! close!:id set-close-mixin:id)
  (begin
    (define close!- (box #f))
    (define (set-close!- close) (set-box! close!- close))
    (define set-close-mixin (make-closing-proc-mixin set-close!-))
    ;; On η-expansion of close!: close! can be #f until it is set, so
    ;; expand the call to close! (by the time it is called it should
    ;; have the correct value, a procedure).
    (define-syntax (close! stx)
      (syntax-parse stx
        [_:id #'(λ () ((unbox close!-)))]
        [(_) #'((unbox close!-))]))))

(define (hide-caret/selection %)
  ;; not using mixin: after-set-position is a method of text% that is not
  ;; exposed by any interface that text% implements
  (class % (super-new)
    (define/augment (after-set-position)
      (send this hide-caret (= (send this get-start-position)
                               (send this get-end-position)))
      (inner (void) after-set-position))))
