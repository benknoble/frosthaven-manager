#lang racket

(provide
 (contract-out
  [struct player ([name string?]
                  [max-hp positive-integer?]
                  [current-hp natural-number/c]
                  [xp natural-number/c]
                  [conditions (listof condition?)]
                  [initiative initiative?]
                  [loot (listof loot-card?)]
                  [summons (listof summon?)])]
  [make-player (-> string? positive-integer? player?)]
  [player-update-name (-> string? (-> player? player?))]
  [player-act-on-hp (-> (-> natural-number/c number?)
                        (-> player? player?))]
  [player-act-on-max-hp (-> (-> natural-number/c number?)
                            (-> player? player?))]
  [player-act-on-xp (-> (-> natural-number/c natural-number/c)
                        (-> player? player?))]
  [player-remove-condition (-> condition? (-> player? player?))]
  [player-add-condition (-> condition? (-> player? player?))]
  [player-condition-handler (-> (list/c condition? boolean?)
                                (-> player? player?))]
  [player-afflicted-by? (-> condition? (-> player? boolean?))]
  [player-dead? (-> player? boolean?)]
  [player-at-max-health? (-> player? boolean?)]
  [player-set-initiative (-> player? initiative? player?)]
  [player-clear-initiative (-> player? player?)]
  [player-add-loot (-> loot-card? (-> player? player?))]
  [player->hp-text (-> player? string?)]
  [player-conditions* (-> player? (listof condition?))]
  [struct summon ([name string?]
                  [max-hp positive-integer?]
                  [current-hp natural-number/c]
                  [conditions (listof condition?)])]
  [summon-update-name (-> string? (-> summon? summon?))]
  [summon-act-on-hp (-> (-> natural-number/c number?)
                        (-> summon? summon?))]
  [summon-act-on-max-hp (-> (-> natural-number/c number?)
                            (-> summon? summon?))]
  [summon-remove-condition (-> condition? (-> summon? summon?))]
  [summon-add-condition (-> condition? (-> summon? summon?))]
  [summon-condition-handler (-> (list/c condition? boolean?)
                                (-> summon? summon?))]
  [summon-afflicted-by? (-> condition? (-> summon? boolean?))]
  [summon-dead? (-> summon? boolean?)]
  [summon-at-max-health? (-> summon? boolean?)]
  [summon->hp-text (-> summon? string?)]
  [summon-conditions* (-> summon? (listof condition?))]
  [player-summon (-> player? string? positive-integer? player?)]
  [update-player-summon (-> natural-number/c (-> summon? summon?)
                            (-> player? player?))]
  [player-kill-summon (-> natural-number/c (-> player? player?))]))

(require
 racket/serialize
 frosthaven-manager/curlique
 frosthaven-manager/qi/utils
 frosthaven-manager/defns/loot
 frosthaven-manager/defns/scenario)

(serializable-struct player [name max-hp current-hp xp conditions initiative loot summons] #:transparent)
(define (make-player name max-hp)
  (player name max-hp max-hp 0 empty 0 empty empty))

(define ((player-update-name name) p)
  (struct-copy player p [name name]))

(define ((player-act-on-hp proc) p)
  (define new-hp (proc (player-current-hp p)))
  (if (not (>= new-hp 0))
    p
    (struct-copy player p [current-hp new-hp])))

(define ((player-act-on-max-hp proc) p)
  (define new-max-hp (proc (player-max-hp p)))
  (if (not (positive? new-max-hp))
    p
    (struct-copy player p [max-hp new-max-hp])))

(define ((player-act-on-xp proc) p)
  (define new-xp (proc (player-xp p)))
  (if (not (>= new-xp 0))
    p
    (struct-copy player p [xp new-xp])))

(define ((player-remove-condition c) p)
  (define new-conditions (remove* (list c) (player-conditions p)))
  (struct-copy player p [conditions new-conditions]))

(define ((player-add-condition c) p)
  (define new-conditions (cons c (remove* (list c) (player-conditions p))))
  (struct-copy player p [conditions new-conditions]))

(define player-condition-handler
  (match-lambda
    [`(,c #f) (player-remove-condition c)]
    [`(,c #t) (player-add-condition c)]))

(define ((player-afflicted-by? c) p)
  (and (member c (player-conditions p)) #t))

(define (player-dead? p)
  (zero? (player-current-hp p)))

(define (player-at-max-health? p)
  (>= (player-current-hp p) (player-max-hp p)))

(define (player-set-initiative p init)
  (struct-copy player p [initiative init]))

(define (player-clear-initiative p)
  (struct-copy player p [initiative 0]))

(define ((player-add-loot card) p)
  (define loot (player-loot p))
  (struct-copy player p [loot (cons card loot)]))

(define (player->hp-text p)
  (match p
    [(struct* player ([max-hp max] [current-hp current]))
      (~a "HP: " current "/" max)]))

(define (player-conditions* p)
  (sort (player-conditions p) string<=? #:key ~a))

(serializable-struct summon [name max-hp current-hp conditions] #:transparent)

(define ((summon-update-name name) s)
  (struct-copy summon s [name name]))

(define ((summon-act-on-hp proc) s)
  (define new-hp (proc (summon-current-hp s)))
  (if (not (>= new-hp 0))
    s
    (struct-copy summon s [current-hp new-hp])))

(define ((summon-act-on-max-hp proc) s)
  (define new-max-hp (proc (summon-max-hp s)))
  (if (not (positive? new-max-hp))
    s
    (struct-copy summon s [max-hp new-max-hp])))

(define ((summon-remove-condition c) s)
  (define new-conditions (remove* (list c) (summon-conditions s)))
  (struct-copy summon s [conditions new-conditions]))

(define ((summon-add-condition c) s)
  (define new-conditions (cons c (remove* (list c) (summon-conditions s))))
  (struct-copy summon s [conditions new-conditions]))

(define summon-condition-handler
  (match-lambda
    [`(,c #f) (summon-remove-condition c)]
    [`(,c #t) (summon-add-condition c)]))

(define ((summon-afflicted-by? c) s)
  (and (member c (summon-conditions s)) #t))

(define (summon-dead? s)
  (zero? (summon-current-hp s)))

(define (summon-at-max-health? s)
  (>= (summon-current-hp s) (summon-max-hp s)))

(define (summon->hp-text s)
  (match s
    [(struct* summon ([max-hp max] [current-hp current]))
      (~a "HP: " current "/" max)]))

(define (summon-conditions* s)
  (sort (summon-conditions s) string<=? #:key ~a))

(define (player-summon p name hp)
  (struct-copy player p
               [summons (append (player-summons p)
                                (list (summon name hp hp empty)))]))

(define ((update-player-summon i f) p)
  (struct-copy player p
               [summons (list-update (player-summons p) i f)]))

(define ((player-kill-summon i) p)
  (struct-copy player p
               [summons (~> (p) player-summons (list-remove i) 1>)]))
