#lang racket

(provide
  (contract-out
    [elements-cycler (->* ((listof (obs/c element-state/c))
                           (listof element-pics?))
                          ((unconstrained-domain-> (is-a?/c view<%>)))
                          (is-a?/c view<%>))]))
(require frosthaven-manager/elements
         frosthaven-manager/gui/helpers
         frosthaven-manager/gui/render
         frosthaven-manager/manager
         frosthaven-manager/observable-operator
         racket/gui/easy
         racket/gui/easy/contract
         (except-in racket/gui #%app)
         (only-in pict inset pict-width pict-height scale))

(module+ test (require rackunit))

(define (elements-cycler @states es [panel hpanel])
  (apply panel #:stretch '(#f #f) (element-cyclers @states es)))

(define (element-cyclers @states es)
  (map element-cycler @states es))

(define (element-cycler @element-state e)
  (define (make-pict-for-canvas s)
    (~> (s)
        (esc (state->pict e))
        (scale 2/3)
        (inset 3)))
  (pict-canvas @element-state
               make-pict-for-canvas
               #:min-size (@> @element-state
                              {~> make-pict-for-canvas
                                  (-< pict-width pict-height) (>< exact-ceiling)
                                  list})
               #:mixin (handle-element-clicks @element-state)))

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
           (render-popup-menu (current-renderer) pum x y))]
        [else (super on-event e)]))))

(define (make-transition-element-state @state)
  (λ ()
    (<@ @state transition-element-state)))

(module+ test
  (test-case "transitions"
    (define/obs state 'infused)
    (define t (make-transition-element-state state))
    (t)
    (check-equal? (@! state) 'waning)
    (t)
    (check-equal? (@! state) 'unfused)
    (t)
    (check-equal? (@! state) 'infused)))

(define (state->pict e)
  (match-lambda
    ['unfused (element-pics-unfused e)]
    ['infused (element-pics-infused e)]
    ['waning (element-pics-waning e)]
    [_ (element-pics-unfused e)]))

(module+ main
  (define es (elements))
  (define @states (make-states es))
  ;; demo
  (void (render/eventspace
          ;; no separate eventspace: block main until this window closed
          (window (vpanel (elements-cycler @states es)
                          (button "Next Round"
                                  (thunk (for-each (curryr obs-update! wane-element) @states)))))))
  ;; testing errors
  #;(void (obs-update! (car @states) (thunk* 'gibberish))))
