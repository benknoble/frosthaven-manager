#lang racket

(provide
  (contract-out
    [update-loot-deck-and-num-loot-cards
      (-> state? (-> (list/c (or/c 'add 'remove) (listof loot-card?)) any))]
    [update-stickers-per-deck (-> state?
                                  (-> (list/c (or/c 'add 'remove) (listof loot-card?))
                                      any))]
    [build-loot-deck (-> (hash/c (listof loot-card?) natural-number/c)
                         (hash/c (listof loot-card?) natural-number/c)
                         (listof loot-card?))]
    [build-loot-deck! (-> state? any)]
    [give-player-loot (-> state? (-> any/c any))]
    [place-loot-on-bottom (-> state? any)]))

(require frosthaven-manager/observable-operator
         frosthaven-manager/defns
         frosthaven-manager/manager/state
         frosthaven-manager/qi)

(define ((update-loot-deck-and-num-loot-cards s) evt)
  ((loot-picker-updater (state-@cards-per-deck s)) evt)
  (<@ (state-@num-loot-cards s) (case (car evt) [(add) add1] [(remove) sub1])))

;; valid: only called if loot-deck non-empty, loot assigned
(define (take-loot s)
  (<~@ (state-@loot-deck s) (if empty? _ rest)))

(define ((give-player-loot* s) p)
  (define card
    (@! (@~> (state-@loot-deck s) (and (not empty?) first))))
  (if card
    ((player-add-loot card) p)
    p))

(define ((give-player-loot s) k)
  (<~@ (state-@creatures s) (update-players k (give-player-loot* s)))
  (take-loot s))

(define-flow rotate
  (~> (-< rest first) (== _ list) append))

(define (place-loot-on-bottom s)
  (<~@ (state-@loot-deck s) rotate))

(define ((loot-picker-updater @cards-per-loot-deck) evt)
  (define (update cards-per-loot-deck)
    (match evt
      [`(add ,deck) (hash-update cards-per-loot-deck deck add1 0)]
      [`(remove ,deck) (hash-update cards-per-loot-deck deck sub1 0)]))
  (<@ @cards-per-loot-deck update))

(define (update-stickers-per-deck s)
  ;; Same representation, so reuse implementation (for now)
  (loot-picker-updater (state-@stickers-per-loot-deck s)))

(define (build-loot-deck cards-per-loot-deck stickers-per-loot-deck)
  (shuffle
   (flatten
    (for/list ([(deck count) (in-hash cards-per-loot-deck)])
      ;; NOTE assume each card only gets one sticker…
      (define stickers (hash-ref stickers-per-loot-deck deck 0))
      (define-values (to-be-stickered unstickered)
        (cond
          [(<= 0 stickers (length deck)) (split-at deck stickers)]
          [(>= stickers (length deck)) (values deck empty)]
          [else (values empty deck)]))
      (define stickered
        (for/list ([card (in-list to-be-stickered)])
          (match card
            [(money amount) (money (add1 amount))]
            [(material name amount) (material name (map add1 amount))]
            [(herb name amount) (herb name (add1 amount))])))
      (take (shuffle (append stickered unstickered)) count)))))

(define (build-loot-deck! s)
  (:= (state-@loot-deck s) (build-loot-deck (@! (state-@cards-per-deck s))
                                            (@! (state-@stickers-per-loot-deck s)))))
