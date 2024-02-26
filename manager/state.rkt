#lang racket

(provide
  state-deserialize-info
  (contract-out
    #:unprotected-submodule unsafe
    [struct creature ([id any/c]
                      [v (or/c player? monster-group*?)])]
    [struct monster-group* ([active (or/c #f monster-number/c)]
                            [mg monster-group?])]
    [creature-is-mg*? (-> creature? any/c)]
    [struct state ([@mode (obs/c symbol?)]
                   [@level (obs/c level/c)]
                   [@num-players (obs/c num-players/c)]
                   [@creatures (obs/c (listof creature?))]
                   [@type->number-of-cards (obs/c (hash/c loot-type/c natural-number/c))]
                   [@loot-deck (obs/c (listof loot-card?))]
                   [@num-loot-cards (obs/c natural-number/c)]
                   [@elements (listof (obs/c element-state/c))]
                   [@in-draw? (obs/c boolean?)]
                   [@round (obs/c natural-number/c)]
                   [@monster-modifier-deck (obs/c (listof monster-modifier?))]
                   [@monster-discard (obs/c (listof monster-modifier?))]
                   [@player-blesses (obs/c (listof monster-modifier?))]
                   [@curses (obs/c (listof monster-modifier?))]
                   [@blesses (obs/c (listof monster-modifier?))]
                   [@modifier (obs/c (or/c #f monster-modifier? ))]
                   [@monster-prev-discard (obs/c (or/c #f monster-modifier?))]
                   [@info-db (obs/c info-db/c)]
                   [@ability-db (obs/c ability-db/c)]
                   [@ability-decks (obs/c (hash/c string? ability-decks?))]
                   [@prompts (obs/c (listof prompt/c))]
                   [@type->deck (obs/c (hash/c loot-type/c (listof loot-card?)))])]
    [make-state
      (->* ()
           ((maybe-obs/c symbol?)
            (maybe-obs/c level/c)
            (maybe-obs/c num-players/c)
            (maybe-obs/c (listof creature?))
            (maybe-obs/c (hash/c loot-type/c natural-number/c))
            (maybe-obs/c (listof loot-card?))
            (maybe-obs/c natural-number/c)
            (listof (maybe-obs/c element-state/c))
            (maybe-obs/c boolean?)
            (maybe-obs/c natural-number/c)
            (maybe-obs/c (listof monster-modifier?))
            (maybe-obs/c (listof monster-modifier?))
            (maybe-obs/c (listof monster-modifier?))
            (maybe-obs/c (listof monster-modifier?))
            (maybe-obs/c (listof monster-modifier?))
            (maybe-obs/c (or/c #f monster-modifier?))
            (maybe-obs/c (or/c #f monster-modifier?))
            (maybe-obs/c info-db/c)
            (maybe-obs/c ability-db/c)
            (maybe-obs/c (hash/c string? ability-decks?))
            (maybe-obs/c (listof prompt/c))
            (maybe-obs/c (hash/c loot-type/c (listof loot-card?))))
           state?)]
    [state-@env (-> state? (obs/c env/c))]
    [serialize-state (-> state? output-port? void?)]
    [deserialize-state (-> input-port? state?)]
    [copy-state (-> state? state? any)]
    [make-undo (-> state? obs?)]
    [undo! (-> state? obs? any)]
    [undoable? (-> list? any/c)]
    [make-player-creature (-> any/c creature?)]
    [update-players (-> (listof creature?) any/c (-> player? player?) (listof creature?))]
    [update-monster-groups (-> (listof creature?)
                               any/c
                               (-> monster-group? monster-group?)
                               (-> (or/c #f monster-number/c) monster-group? (or/c #f monster-number/c))
                               (listof creature?))]
    [update-all-players (-> (listof creature?) (-> player? player?) (listof creature?))]
    [update-all-monster-groups (-> (listof creature?) (-> monster-group? monster-group?) (listof creature?))]
    [update-player-name (-> state? (-> any/c string? any))]
    [update-player-max-hp (-> state? (-> any/c (-> natural-number/c natural-number/c) any))]
    [creature-initiative (-> (hash/c string? ability-decks?) (-> creature? (or/c +inf.0 initiative?)))]
    [single-monster-event/c contract?]
    [add-monster-event/c contract?]
    [remove-monster-event/c contract?]
    [add-or-remove-monster-group (-> state? (-> (or/c add-monster-event/c remove-monster-event/c) any))]
    [draw-new-card-mid-round-if-needed (-> state? string? any)]
    [initiative-public? (-> boolean? boolean?)]
    [add-prompt (-> state? (-> prompt/c any))]
    [remove-prompt (-> state? (-> natural-number/c prompt/c any))]))

(require racket/serialize
         racket/fasl
         racket/gui/easy/contract
         racket/gui/easy/observable
         frosthaven-manager/observable-operator
         frosthaven-manager/defns
         frosthaven-manager/qi
         frosthaven-manager/monster-db
         frosthaven-manager/manager/ability-decks
         frosthaven-manager/manager/round-prompts
         frosthaven-manager/manager/elements
         frosthaven-manager/parsers/formula)

(serializable-struct creature [id v] #:transparent)
(serializable-struct monster-group* [active mg] #:transparent)

(define-flow creature-is-mg*? (~> creature-v monster-group*?))

;; private observable utilities for this module
(define-flow s@->v (~> struct->vector (vector-drop 1) (vector-map @!* _)))
(define (@!* o)
  (cond
    [(list? o) (map @! o)]
    [else (@! o)]))

(struct state
        [@mode
         @level
         @num-players
         @creatures
         @type->number-of-cards
         @loot-deck
         @num-loot-cards
         @elements
         @in-draw?
         @round
         @monster-modifier-deck
         @monster-discard
         @player-blesses
         @curses
         @blesses
         @modifier
         @monster-prev-discard
         @info-db
         @ability-db
         @ability-decks
         @prompts
         @type->deck]
        #:transparent ;; for struct->vector
        #:property prop:serializable
        (make-serialize-info
          s@->v
          #'state-deserialize-info
          #f
          (or (current-load-relative-directory) (current-directory))))

(define (make-state [@mode (@ 'start)]
                    [@level (@ 0)]
                    [@num-players (@ 2)]
                    [@creatures (@ empty)]
                    [@type->number-of-cards (@ (hash))]
                    [@loot-deck (@ empty)]
                    [@num-loot-cards (@ 0)]
                    [@elements (make-states '(fire ice air earth light dark))]
                    [@in-draw? (@ #f)]
                    [@round (@ 1)]
                    [@monster-modifier-deck (@ (shuffle monster-modifier-deck))]
                    [@monster-discard (@ empty)]
                    [@player-blesses (@ empty)]
                    [@curses (@ monster-curse-deck)]
                    [@blesses (@ bless-deck)]
                    [@modifier (@ #f)]
                    [@monster-prev-discard (@ #f)]
                    [@info-db (@ (hash))]
                    [@ability-db (@ (hash))]
                    [@ability-decks (@ (hash))]
                    [@prompts (@ empty)]
                    [@type->deck (@ standard-loot-deck)])
  (state (@ @mode)
         (@ @level)
         (@ @num-players)
         (@ @creatures)
         (let* ([@h (@ @type->number-of-cards)]
                [h (@! @h)])
           (cond
             [(hash-empty? h) @h]
             [(andmap loot-type/c (hash-keys h)) @h]
             [(andmap (listof loot-card?) (hash-keys h))
              ;; convert old datatype, with some assumptions
              (@ (for/hash ([(deck count) (in-hash-values h)]
                            #:unless (empty? deck))
                   (values (card->type (first deck)) count)))]))
         (@ @loot-deck)
         (@ @num-loot-cards)
         (map @ @elements)
         (@ @in-draw?)
         (@ @round)
         (@ @monster-modifier-deck)
         (@ @monster-discard)
         (@ @player-blesses)
         (@ @curses)
         (@ @blesses)
         (@ @modifier)
         (@ @monster-prev-discard)
         (@ @info-db)
         (@ @ability-db)
         (@ @ability-decks)
         (@ @prompts)
         (@ @type->deck)))

(define (state-@env s)
  (obs-combine (λ (c l) (hash "C" c "L" l))
               (state-@num-players s)
               (state-@level s)))

(define state-deserialize-info
  (make-deserialize-info
    make-state
    (thunk
      (error 'state "cycles not supported"))))

(define (serialize-state s out)
  (s-exp->fasl (serialize s) out))

(define (deserialize-state in)
  (deserialize (fasl->s-exp in)))

(module+ test
  (require rackunit)
  ;; dynamic-require: break module cycle
  ;; frosthaven-manager/manager/db -> frosthaven-manager/manager/state
  (define init-dbs (dynamic-require 'frosthaven-manager/manager/db 'init-dbs))
  ;; frosthaven-manager/manager/modifier-decks -> frosthaven-manager/manager/state
  (define do-bless-player (dynamic-require 'frosthaven-manager/manager/modifier-decks 'do-bless-player))

  (test-case
    "Serializable state"

    (define s (make-state))
    (void (init-dbs default-monster-db s))
    (define-values (readable writable) (make-pipe (expt 2 10)))
    (thread (thunk (serialize-state s writable)))
    (define s* (deserialize-state readable))

    (check-equal? (s@->v s*) (s@->v s)))

  (test-case
    "Copyable state"

    (define s1 (make-state))
    (define s2 (make-state))
    (void (init-dbs default-monster-db s1)
          ((do-bless-player s1))
          ;;
          (copy-state s1 s2))

    (check-equal? (s@->v s2) (s@->v s1))))

(define (copy-state from to)
  (:=     (state-@mode to)
      (@! (state-@mode from)))
  (:=     (state-@level to)
      (@! (state-@level from)))
  (:=     (state-@num-players to)
      (@! (state-@num-players from)))
  (:=     (state-@creatures to)
      (@! (state-@creatures from)))
  (:=     (state-@type->number-of-cards to)
      (@! (state-@type->number-of-cards from)))
  (:=     (state-@loot-deck to)
      (@! (state-@loot-deck from)))
  (:=     (state-@num-loot-cards to)
      (@! (state-@num-loot-cards from)))
  (for ([to-element (in-list (state-@elements to))]
        [from-element (in-list (state-@elements from))])
    (:=     to-element
        (@! from-element)))
  (:=     (state-@in-draw? to)
      (@! (state-@in-draw? from)))
  (:=     (state-@round to)
      (@! (state-@round from)))
  (:=     (state-@monster-modifier-deck to)
      (@! (state-@monster-modifier-deck from)))
  (:=     (state-@monster-discard to)
      (@! (state-@monster-discard from)))
  (:=     (state-@player-blesses to)
      (@! (state-@player-blesses from)))
  (:=     (state-@curses to)
      (@! (state-@curses from)))
  (:=     (state-@blesses to)
      (@! (state-@blesses from)))
  (:=     (state-@modifier to)
      (@! (state-@modifier from)))
  (:=     (state-@monster-prev-discard to)
      (@! (state-@monster-prev-discard from)))
  (:=     (state-@info-db to)
      (@! (state-@info-db from)))
  (:=     (state-@ability-db to)
      (@! (state-@ability-db from)))
  (:=     (state-@ability-decks to)
      (@! (state-@ability-decks from)))
  (:=     (state-@prompts to)
      (@! (state-@prompts from)))
  (:=     (state-@type->deck to)
      (@! (state-@type->deck from))))

;;; UNDO
(define (make-undo s)
  (define/obs @undo (list (s@->v s)))
  (define (push-state . _args)
    (<~@ @undo
         (~>> (-< _ (~> length (min 20)))
              take
              (cons (s@->v s)))))
  ;; Only observe meaningful changes in state. Observing every change means
  ;; that, for example, it takes multiple undo! calls to undo assigning loot or
  ;; starting the round.
  (for ([element (state-@elements s)])
    (obs-observe! element push-state))
  (for ([field (list state-@level
                     state-@creatures
                     state-@modifier
                     state-@curses
                     state-@blesses
                     state-@in-draw?)])
    (obs-observe! (field s) push-state))
  @undo)

(define (undo! s @undo)
  ;; IMPORTANT
  ;; The _saved_ copy `undo` holds the undo stack as of RIGHT NOW. When we call
  ;; copy-state below, we will trigger changes in s that will (ostensibly) push
  ;; more chagnes to @undo. But when we update @undo to be the tail of undo, we
  ;; will discard those (spurious) changes _and_ the top of the stack which we
  ;; are copying.
  (define undo (@! @undo))
  ;; INVARIANT
  ;; The `undo` should never be empty. If it is, we've lost one state. Why? When
  ;; we push states in reaction to observable changes, we are pushing the _new_
  ;; state, not the old. So the only reference to the old state is "underneath"
  ;; the current state. This also means that the state to restore is actually
  ;; the 2nd element of the stack. There could be no second element, though.
  (when (undoable? undo)
    (define to-restore (second undo))
    (define new-state (apply make-state (vector->list to-restore)))
    (copy-state new-state s)
    (:= @undo (cdr undo))))

(define (undoable? undo)
  (not (empty? (cdr undo))))

(define (make-player-creature i)
  (creature i (make-player "" 1)))

(define (update-players creatures k f)
  (define (maybe-update-player e)
    (if (~> (e) (-< creature-id creature-v) (and% (eq? k) player?))
      (creature k (f (creature-v e)))
      e))
  (map maybe-update-player creatures))

(define (update-monster-groups creatures k f [fn (flow 1>)])
  (define (maybe-update-monster-group e)
    (if (~> (e)
            (-< creature-id creature-v)
            (and% (eq? k) monster-group*?))
      (let* ([mg* (creature-v e)]
             [n (monster-group*-active mg*)]
             [mg (monster-group*-mg mg*)]
             [new-mg (f mg)]
             [new-n (fn n new-mg)])
        (creature k (monster-group* new-n new-mg)))
      e))
  (map maybe-update-monster-group creatures))

(define (update-all-players creatures f)
  (define (update-only-player c)
    (match c
      [(creature id (? player? p)) (creature id (f p))]
      [c c]))
  (map update-only-player creatures))

(define (update-all-monster-groups creatures f)
  (define (update-only-monster-group c)
    (match c
      [(creature id (monster-group* n mg))
       (creature id (monster-group* n (f mg)))]
      [c c]))
  (map update-only-monster-group creatures))

(define ((update-player-name s) k name)
  (<~@ (state-@creatures s) (update-players k (player-update-name name))))

(define ((update-player-max-hp s) k f)
  (<~@ (state-@creatures s) (update-players k (player-act-on-max-hp f))))

(define ((monster-group-initiative ads) mg)
  (~> (ads mg)
      (== _ monster-group-set-name)
      hash-ref
      ability-decks-current
      (switch
        [monster-ability? monster-ability-initiative]
        [else +inf.0])))

(define (monster-group*-initiative ads)
  (flow (~> monster-group*-mg (esc (monster-group-initiative ads)))))

(define (creature-initiative ads)
  (flow (~> creature-v
            (switch
              [player? player-initiative]
              [monster-group*? (esc (monster-group*-initiative ads))]))))

(module+ test
  (require frosthaven-manager/testfiles/data)
  (test-case
    "Initiative"
    (define s (make-sample-state))
    (define (get-creature id)
      (findf (flow (~> creature-id (= id)))
             (@! (state-@creatures s))))
    (let ([ads (@! (state-@ability-decks s))])
      (check-equal? ((creature-initiative ads) (get-creature jack)) 0)
      (check-equal? ((creature-initiative ads) (get-creature frigg)) 67)
      (check-equal? ((creature-initiative ads) (get-creature archers)) +inf.0))
    (<@ (state-@ability-decks s) (update-ability-decks ability-decks-draw-next))
    (let ([ads (@! (state-@ability-decks s))])
      (define expected
        (~>> (archers)
             get-creature creature-v monster-group*-mg
             monster-group-set-name
             (hash-ref ads)
             ability-decks-current
             monster-ability-initiative))
      (check-equal? ((creature-initiative ads) (get-creature archers)) expected))))

;; (-> mg (-> creature bool))
(define-flow creature-is-mg~?
  (clos (~>
          (== _ (and creature-is-mg*? (~> creature-v monster-group*-mg)))
          equal?)))

(define single-monster-event/c
  (or/c
    (list/c 'set 'from string? 'to string?)
    (list/c 'monster 'from monster-info? 'to monster-info?)
    (list/c 'include? monster-number/c 'to boolean?)
    (list/c 'elite? monster-number/c 'to boolean?)
    (list/c 'level level/c)))

(define add-monster-event/c
  (list/c 'add monster-group?))

(define remove-monster-event/c
  (list/c 'remove monster-group?))

(define ((add-or-remove-monster-group s) evt)
  (match evt
    [`(add ,mg)
      (<@ (state-@creatures s)
          (λ (creatures)
            (define next-id (~> (creatures) (sep creature-id) max add1))
            (define selection
              (~> (mg) monster-group-monsters
                  (and (not empty?) (~> first monster-number))))
            (define c (creature next-id (monster-group* selection mg)))
            (append creatures (list c))))]
    [`(remove ,mg) (<~@ (state-@creatures s) (remf (creature-is-mg~? mg) _))]))

(define (draw-new-card-mid-round-if-needed s set)
  ;; mid-round, we've added a monster, and they didn't already have a card
  ;; caller must check "added a monster" condition, which varies
  (<@ (state-@ability-decks s)
      (λ (ads)
        (cond
          [(and (@! (state-@in-draw? s))
                (~> (ads) (hash-ref set) (not ability-decks-current)))
           (hash-update ads set ability-decks-draw-next)]
          [else ads]))))

(define (initiative-public? in-draw?)
  in-draw?)

(define ((add-prompt s) p)
  (<~@ (state-@prompts s) (cons p _)))

(define ((remove-prompt s) i p)
  (define-values (new-ps p2)
    (list-remove (@! (state-@prompts s)) i))
  (when (equal? p p2)
    (:= (state-@prompts s) new-ps)))
