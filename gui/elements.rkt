#lang racket

(provide
  (contract-out
    [element-state/c contract?]
    [elements-cycler (->* ((listof element-pics?))
                          ((unconstrained-domain-> (is-a?/c view<%>)))
                          (values (listof (obs/c element-state/c))
                                  (is-a?/c view<%>)))]
    [wane-element (-> element-state/c element-state/c)]))

(define element-state/c (or/c 'unfused 'infused 'waning))

(require racket/gui/easy
         frosthaven-manager/observable-operator
         racket/gui/easy/contract
         racket/gui
         frosthaven-manager/gui/render
         frosthaven-manager/gui/helpers

         frosthaven-manager/elements
         (only-in pict inset))

(define (elements-cycler es [panel hpanel])
  (define-values (states views) (element-cyclers es))
  (values states (apply panel #:stretch '(#f #f) views)))

(define (make-transition-element-state @state)
  (λ ()
    (<@ @state transition-element-state)))

(define (handle-element-clicks @state)
  (define cycle-element (make-transition-element-state @state))
  (mixin (canvas<%>) ()
    (super-new)
    (define/override (on-event e)
      (case (send e get-event-type)
        [(left-down) (cycle-element)]
        [(right-down)
         (define-values (x y)
           (translate-to-top-coords this
                                    (renderer-root (current-renderer))
                                    (send e get-x)
                                    (send e get-y)))
         (when (and x y)
           (define pum
             (popup-menu (menu "Transition to…"
                               (menu-item "Infused" (λ () (:= @state 'infused)))
                               (menu-item "Waning" (λ () (:= @state 'waning)))
                               (menu-item "Unfused" (λ () (:= @state 'unfused))))))
           (render-popup-menu (current-renderer) pum x y))]))))

(define (element-cycler e)
  (define/obs @element-state 'unfused)
  (define (make-pict-for-canvas s)
    (inset ((state->pict e) s) (+ 3 (/ size 3)) 3 0 0))
  (define cycle-element (make-transition-element-state @element-state))
  (define pict-view
    (pict-canvas @element-state
                 make-pict-for-canvas
                 #:min-size (list (+ 6 size) (+ 6 size))
                 #:mixin (handle-element-clicks @element-state)))
  (define cycler-view
    (group
      (element-pics-name e)
      #:stretch '(#f #f)
      pict-view
      (button (@> @element-state state->text) cycle-element)))
  (values @element-state cycler-view))

(define (state->pict e)
  (match-lambda
    ['unfused (element-pics-unfused e)]
    ['infused (element-pics-infused e)]
    ['waning (element-pics-waning e)]
    [_ (element-pics-unfused e)]))

(define state->text
  (match-lambda
    ['unfused "Infuse"]
    ['infused "Wane"]
    ['waning "Unfuse"]
    [_ "Infuse"]))

(define transition-element-state
  (match-lambda
    ['unfused 'infused]
    ['infused 'waning]
    ['waning 'unfused]
    [_ 'infused]))

(define wane-element
  (match-lambda
    ['infused 'waning]
    ['waning 'unfused]
    [_ 'unfused]))

(define (element-cyclers es)
  (for/fold ([@states empty]
             [views empty]
             #:result (values (reverse @states)
                              (reverse views)))
    ([e (in-list es)])
    (define-values (@state view) (element-cycler e))
    (values (cons @state @states)
            (cons view views))))

(module+ main
  (define-values (@states view) (elements-cycler elements))
  ;; demo
  (void (render/eventspace
          ;; no separate eventspace: block main until this window closed
         (window (vpanel view
                         (button "Next Round"
                                 (thunk (for-each (curryr obs-update! wane-element) @states)))))))
  ;; testing errors
  #;(void (obs-update! (car @states) (thunk* 'gibberish))))
