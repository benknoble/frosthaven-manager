#lang racket

(provide (contract-out
           [struct @player-input ([id any/c]
                                  [@name (obs/c string?)]
                                  [@hp (obs/c positive-integer?)])]
           [player-input-views (-> (obs/c (integer-in 1 max-players))
                                   (values (obs/c (listof @player-input?))
                                           (is-a?/c view<%>)))]))

(require racket/gui/easy
         racket/gui/easy/operator
         racket/gui/easy/contract
         "defns.rkt")

(define (player-input-views @num-players)
  (define @ps (~> @num-players (λ (np)
                                 (map (thunk* (make-@player-input)) (range np)))))
  (define v
    (list-view @ps
               #:key @player-input-id
               (match-lambda**
                 [(id (app obs-peek (@player-input id @name @hp)))
                  (player-input-view @name @hp)])))
  (values @ps v))

(define (player-input-view @name @hp)
  (define input-view
    (hpanel
      (input #:label "Name" @name
             (match-lambda**
               [(_ (? string? s)) (:= @name s)]
               [(_ _) (void)]))
      (button "-" (thunk (if (obs-peek (~> @hp (curry = 1)))
                           (void)
                           (<~ @hp sub1))))
      (text (~> @hp ~a))
      (button "+" (thunk (<~ @hp add1)))))
  input-view)

(struct @player-input [id @name @hp] #:transparent)
(define (make-@player-input)
  (define/obs @name "")
  (define/obs @hp 1)
  (@player-input (gensym) @name @hp))

(module+ main
  (define-values (@ps view) (player-input-views (@ 3)))
  (void (render (window (vpanel
                          view
                          (button "Debug" (thunk (displayln @ps))))))))
