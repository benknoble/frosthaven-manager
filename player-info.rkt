#lang racket

(provide (contract-out
           [struct @player ([id any/c]
                            [@name (obs/c string?)]
                            [@hp (obs/c positive-integer?)])]
           [player-info-views (-> (obs/c (integer-in 1 max-players))
                                  (values (obs/c (listof @player?))
                                          (is-a?/c view<%>)))]))

(require racket/gui/easy
         racket/gui/easy/operator
         racket/gui/easy/contract
         "defns.rkt")

(define (player-info-views @num-players)
  (define @ps (~> @num-players (λ (np)
                                (map (thunk* (make-@player)) (range np)))))
  (define v
    (list-view @ps
               #:key @player-id
               (match-lambda**
                 [(id (app obs-peek (@player id @name @hp)))
                  (player-info-view @name @hp)])))
  (values @ps v))

(define (player-info-view @name @hp)
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

(struct @player [id @name @hp] #:transparent)
(define (make-@player)
  (define/obs @name "")
  (define/obs @hp 1)
  (@player (gensym) @name @hp))

(module+ main
  (define-values (@ps view) (player-info-views (@ 3)))
  (void (render (window (vpanel
                          view
                          (button "Debug" (thunk (displayln @ps))))))))
