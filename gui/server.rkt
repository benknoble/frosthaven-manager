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
  (require racket/runtime-path
           frosthaven-manager/defns
           frosthaven-manager/qi
           frosthaven-manager/monster-db)
  (define-runtime-path more-monsters "../testfiles/sample-bestiary-import.rkt")
  ;; gui/manager depends on gui/server…
  (define manager (dynamic-require 'frosthaven-manager/gui/manager 'manager))
  (define-values (info _abilities) (get-dbs more-monsters))
  (define mg (make-monster-group (~> (info) (hash-ref "archer") (hash-ref "hynox archer"))
                                 0
                                 '([1 . #t] [2 . #f] [3 . #t])
                                 (hash)))
  (define boss (make-monster-group (~> (info) (hash-ref "boss") (hash-ref "giant squid"))
                                   0
                                   '([1 . #f])
                                   (hash "C" 2)))
  (define s (make-state (@ 'play)))
  (void
   (init-dbs more-monsters s)
   (:= (state-@num-players s) 2)
   (:= (state-@creatures s)
       (list (creature 0 (~> ((make-player "Jack Skellington" 8))
                             (player-summon "Corpse Bro" 4)))
             (creature 1 (~> ((player "Frigg" 12 10 3 (list muddle ward) 67 empty empty))
                             (player-summon "Banner of Courage" 7)))
             (creature 2 (monster-group* 1 mg))
             (creature 3 (monster-group* 1 boss))))
   (launch-server s)
   (render/eventspace (manager s))))
