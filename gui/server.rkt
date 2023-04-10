#lang racket

(provide
  (contract-out
    [launch-server (-> state? renderer?)]))

(require racket/gui
         racket/gui/easy
         frosthaven-manager/observable-operator
         frosthaven-manager/gui/render
         frosthaven-manager/manager
         (prefix-in server: frosthaven-manager/server))

(define (launch-server s)
  (define gui-eventspace (current-eventspace))
  (define (handler evt)
    (parameterize ([current-eventspace gui-eventspace])
      (handle s evt)))
  (define-values (addr stopper)
    (server:launch-server s handler))
  (define stop (box stopper))
  (define/obs @addr addr)
  (define (restart)
    ((unbox stop))
    (define-values (addr stopper)
      (server:launch-server s handler))
    (set-box! stop stopper)
    (:= @addr addr))
  (with-closing-custodian/eventspace
    (render/eventspace
      #:eventspace closing-eventspace
      (window
        #:mixin close-custodian-mixin
        (text (@~> @addr (~a "Server: " _)))
        (button "Restart Server" restart)))))

(define (handle _s evt)
  (evt))

(module+ main
  (define manager (dynamic-require 'frosthaven-manager/gui/manager 'manager))
  (define s (make-state (@ 'play)))
  (launch-server s)
  (render/eventspace (manager s)))
