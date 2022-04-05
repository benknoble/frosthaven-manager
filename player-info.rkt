#lang racket

(provide (contract-out
           [player-info-views (-> (integer-in 1 max-players)
                                  (values (listof (obs/c player?))
                                          (is-a?/c view<%>)))]))

(require racket/gui/easy
         racket/gui/easy/operator
         racket/gui/easy/contract
         "defns.rkt")

(define (player-info-views num-players)
  (define-values (@ps vs) (player-infos num-players))
  (values @ps (apply vpanel vs)))

(define (player-infos num-players)
  (for/fold ([@ps empty]
             [vs empty]
             #:result (values (reverse @ps)
                              (reverse vs)))
    ([_ (in-range num-players)])
    (define-values (@p v) (player-info-view))
    (values (cons @p @ps)
            (cons v vs))))

(define (player-info-view)
  (define/obs @player-name "")
  (define/obs @player-hp 1)
  (define @player (obs-combine make-player @player-name @player-hp))
  (define input-view
    (hpanel
      (input #:label "Name" @player-name
             (match-lambda**
               [(_ (? string? s)) (:= @player-name s)]
               [(_ _) (void)]))
      (button "-" (thunk (if (obs-peek (~> @player-hp (curry = 1)))
                           (void)
                           (<~ @player-hp sub1))))
      (text (~> @player-hp ~a))
      (button "+" (thunk (<~ @player-hp add1)))))
  (values @player input-view))

(module+ main
  (define-values (@ps view) (player-info-views 3))
  (void (render (window (vpanel
                          view
                          (button "Debug" (thunk (displayln @ps))))))))
