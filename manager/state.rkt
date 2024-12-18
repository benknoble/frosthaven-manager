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
    [struct state ([@level (obs/c level/c)]
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
                   [@bestiary-path (obs/c (or/c #f path-string?))]
                   [@ability-decks (obs/c (hash/c string? ability-decks?))]
                   [@prompts (obs/c (listof prompt/c))]
                   [@type->deck (obs/c (hash/c loot-type/c (listof loot-card?)))])]
    [make-state
      (->* ()
           ((maybe-obs/c level/c)
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
            (maybe-obs/c (or/c #f path-string?))
            (maybe-obs/c (hash/c string? ability-decks?))
            (maybe-obs/c (listof prompt/c))
            (maybe-obs/c (hash/c loot-type/c (listof loot-card?))))
           state?)]
    [state-@env (-> state? (obs/c env/c))]
    [state-@info-db (-> state? (obs/c info-db/c))]
    [state-@ability-db (-> state? (obs/c ability-db/c))]
    [state-@active-monster-groups (-> state? (obs/c (listof monster-group?)))]
    [serialize-state (-> state? output-port? void?)]
    [deserialize-state (-> input-port? state?)]
    [copy-state (-> state? state? any)]
    [undo? (-> any/c boolean?)]
    [make-undo (-> state? undo?)]
    [undo! (-> state? undo? any)]
    [undoable? (-> undo? (obs/c boolean?))]
    [make-player-creature (-> any/c creature?)]
    [setup-players (-> state? any)]
    [update-players (-> (listof creature?) any/c (-> player? player?) (listof creature?))]
    [update-monster-groups (->* {(listof creature?)
                                 any/c
                                 (-> monster-group? monster-group?)}
                                {(-> (or/c #f monster-number/c) monster-group? (or/c #f monster-number/c))}
                                (listof creature?))]
    [kill-monster (-> state? any/c monster-number/c any)]
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

(require frosthaven-manager/defns
         frosthaven-manager/manager/ability-decks
         frosthaven-manager/manager/elements
         frosthaven-manager/manager/round-prompts
         frosthaven-manager/monster-db
         frosthaven-manager/observable-operator
         frosthaven-manager/parsers/formula
         frosthaven-manager/qi/utils
         racket/fasl
         racket/gui/easy/contract
         racket/gui/easy/observable
         racket/serialize)

(serializable-struct creature [id v] #:transparent)
(serializable-struct monster-group* [active mg] #:transparent)

(define creature-is-mg*? {~> creature-v monster-group*?})

;; private observable utilities for this module
(define s@->v {~> struct->vector (vector-drop 1) (vector-map @!* _)})
(define (@!* o)
  (cond
    [(list? o) (map @! o)]
    [else (@! o)]))

(struct state
        [@level
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
         @bestiary-path
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

(define (make-state [@level (@ 0)]
                    [@num-players (@ 2)]
                    [@creatures (@ (list (creature 0 (make-player "" 1))
                                         (creature 1 (make-player "" 1))))]
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
                    [@bestiary-path (@ #f)]
                    [@ability-decks (@ (hash))]
                    [@prompts (@ empty)]
                    [@type->deck (@ standard-loot-deck)])
  (state (@ @level)
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
         (@ @bestiary-path)
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

(module+ test-helpers
  (provide get-creature
           get-ability-decks)

  (define (get-creature s id)
    (findf {~> creature-id (= id)}
           (@! (state-@creatures s))))

  (define get-ability-decks
    {~> (-< (~> 1> state-@ability-decks @!)
            (~> get-creature creature-v monster-group*-mg monster-group-set-name))
        hash-ref}))

(module+ test
  (require rackunit
           (submod ".." test-helpers))
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
  (:=     (state-@bestiary-path to)
      (@! (state-@bestiary-path from)))
  (:=     (state-@ability-decks to)
      (@! (state-@ability-decks from)))
  (:=     (state-@prompts to)
      (@! (state-@prompts from)))
  (:=     (state-@type->deck to)
      (@! (state-@type->deck from))))

(define (state-@info-db s)
  (@> (state-@bestiary-path s)
      {(if _
           (~> get-dbs 1>)
           (gen (hash)))}))

(define (state-@ability-db s)
  (@> (state-@bestiary-path s)
      {(if _
           (~> get-dbs 2>)
           (gen (hash)))}))

(define (state-@active-monster-groups s)
  (@> (state-@creatures s)
      {~>> (filter creature-is-mg*?)
           (map {~> creature-v monster-group*-mg})}))

(struct undo [@undo lock])

;;; UNDO
(define (make-undo s)
  (define lock (make-semaphore 1))
  (define/obs @undo (list (s@->v s)))
  (define (push-state . _args)
    (<@ @undo
        (λ (undo)
          (define to-save (s@->v s))
          ;; INVARIANT: undo is never empty (see undo!)
          (define top (car undo))
          ;; don't save an undo state if the state hasn't changed
          (cond
            [(equal? to-save top) undo]
            [else (on (undo)
                    (~>> (-< _ (~> length (min 20)))
                         take
                         (cons to-save)))]))))
  (define undo-thread
    (thread
     (thunk
      (let loop ()
        (call-with-semaphore
         lock
         (λ (args) (apply push-state args))
         #f
         (thread-receive))
        (loop)))))
  (define (send-state . args)
    ;; thread-send is non-blocking, so the observers that call this will return
    ;; quickly. Meanwhile, undo-thread will keep processing the events.
    (thread-send undo-thread args))
  ;; Only observe meaningful changes in state. Observing every change means
  ;; that, for example, it takes multiple undo! calls to undo assigning loot or
  ;; starting the round.
  (for ([element (state-@elements s)])
    (obs-observe! element send-state))
  (for ([field (list state-@level
                     state-@creatures
                     state-@modifier
                     state-@curses
                     state-@blesses
                     state-@in-draw?)])
    (obs-observe! (field s) send-state))
  (undo @undo lock))

(define (undo! s u)
  (call-with-semaphore
   (undo-lock u)
   (thunk
    ;; IMPORTANT
    ;; The _saved_ copy `undo` holds the undo stack as of RIGHT NOW. When we call
    ;; copy-state below, we will trigger changes in s that will (ostensibly) push
    ;; more chagnes to @undo. But when we update @undo to be the tail of undo, we
    ;; will discard those (spurious) changes _and_ the top of the stack which we
    ;; are copying.
    (define @undo (undo-@undo u))
    (define undo (@! @undo))
    ;; INVARIANT
    ;; The `undo` should never be empty. If it is, we've lost one state. Why? When
    ;; we push states in reaction to observable changes, we are pushing the _new_
    ;; state, not the old. So the only reference to the old state is "underneath"
    ;; the current state. This also means that the state to restore is actually
    ;; the 2nd element of the stack. There could be no second element, though.
    (when (@! (undoable? u))
      (define to-restore (second undo))
      (define new-state (apply make-state (vector->list to-restore)))
      (copy-state new-state s)
      (:= @undo (cdr undo))))))

(define (undoable? u)
  (@> (undo-@undo u) {~> cdr (not empty?)}))

(define (make-player-creature i)
  (creature i (make-player "" 1)))

(define (setup-players s)
  (define n-desired-players (@! (state-@num-players s)))
  (define cs (@! (state-@creatures s)))
  (define-values (ps mgs)
    (~> (cs) sep
        (partition
         [(~> creature-v player?) collect]
         [(~> creature-v monster-group*?) collect])))
  (define n-actual-players (length ps))
  (cond
    [(< n-actual-players n-desired-players)
     (define next-id (~> (cs) (sep creature-id) (rectify -1) max add1))
     (<@ (state-@creatures s)
         {(append (build-list (- n-desired-players n-actual-players)
                              (λ (i)
                                (make-player-creature (+ next-id i)))))})]
    [(> n-actual-players n-desired-players)
     ;; throw away old values
     (:= (state-@creatures s) (append (take ps n-desired-players) mgs))]))

(define (update-players creatures k f)
  (match creatures
    [(cons (creature (== k) (? player? v)) creatures)
     (cons (creature k (f v)) creatures)]
    [(cons c creatures)
     (cons c (update-players creatures k f))]
    [cs cs]))

(define (update-monster-groups creatures k f [fn {1>}])
  (match creatures
    [(cons (creature (== k) (? monster-group*? mg*)) creatures)
     (define n (monster-group*-active mg*))
     (define mg (monster-group*-mg mg*))
     (define new-mg (f mg))
     (define new-n (fn n new-mg))
     (cons (creature k (monster-group* new-n new-mg)) creatures)]
    [(cons c creatures)
     (cons c (update-monster-groups creatures k f fn))]
    [cs cs]))

(define (kill-monster s monster-group-id monster-number)
  ;; g records the result of monster-group-remove to avoid having to traverse
  ;; state-@creatures again to find out what to do; with more efficient data
  ;; structures, this would likely be unnecessary.
  (define g (box #f))
  (<@ (state-@creatures s)
      {(update-monster-groups
        monster-group-id
        (λ (mg)
          (define new-mg ((monster-group-remove monster-number) mg))
          (set-box! g new-mg)
          new-mg)
        {~> 2> monster-group-first-monster})})
  (define new-mg (unbox g))
  (when (and new-mg (~> (new-mg) monster-group-monsters empty?))
    ((add-or-remove-monster-group s) `(remove ,new-mg))))

(define (update-all-players creatures f)
  (define n-players (count {~> creature-v player?} creatures))
  (let loop ([creatures creatures]
             [seen-players 0])
    (if (= seen-players n-players)
        creatures
        (match creatures
          [(cons (creature id (? player? p)) creatures)
           (cons (creature id (f p)) (loop creatures (add1 seen-players)))]
          [(cons c creatures)
           (cons c (loop creatures seen-players))]
          [cs cs]))))

(define (update-all-monster-groups creatures f)
  (define (update-only-monster-group c)
    (match c
      [(creature id (monster-group* n mg))
       (creature id (monster-group* n (f mg)))]
      [c c]))
  (map update-only-monster-group creatures))

(define ((update-player-name s) k name)
  (<@ (state-@creatures s) {(update-players k (player-update-name name))}))

(define ((update-player-max-hp s) k f)
  (<@ (state-@creatures s) {(update-players k (player-act-on-max-hp f))}))

(define ((monster-group-initiative ads) mg)
  (~> (ads mg)
      (== _ monster-group-set-name)
      (hash-ref #f)
      (if _
          (~> ability-decks-current
              (switch
                [monster-ability? monster-ability-initiative]
                [else +inf.0]))
          +inf.0)))

(define (monster-group*-initiative ads)
  {~> monster-group*-mg (esc (monster-group-initiative ads))})

(define (creature-initiative ads)
  {~> creature-v
      (switch
        [player? player-initiative]
        [monster-group*? (esc (monster-group*-initiative ads))])})

(module+ test
  (require frosthaven-manager/testfiles/data)
  (test-case
    "Initiative"
    (define s (make-sample-state))
    (let ([ads (@! (state-@ability-decks s))])
      (check-equal? ((creature-initiative ads) (get-creature s jack)) 0)
      (check-equal? ((creature-initiative ads) (get-creature s frigg)) 67)
      (check-equal? ((creature-initiative ads) (get-creature s archers)) +inf.0))
    (<@ (state-@ability-decks s) (update-ability-decks {~> 2> ability-decks-draw-next}))
    (let ([ads (@! (state-@ability-decks s))])
      (define expected
        (~>> (s archers)
             get-ability-decks
             ability-decks-current
             monster-ability-initiative))
      (check-equal? ((creature-initiative ads) (get-creature s archers)) expected))))

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
     (define set (monster-group-set-name mg))
     ;; valid: typically called from a dialog closer
     (define abilities (hash-ref (@! (state-@ability-db s)) set))
     ;; need to update ability-decks first so that when creatures is updated
     ;; anything using (creature-initiative …) will work. only update
     ;; ability-decks if there isn't one already, though!
     (<@ (state-@ability-decks s)
         {(hash-update
           set
           {(or _ (gen (ability-decks #f (shuffle abilities) empty)))}
           #f)})
     (<@ (state-@creatures s)
         (λ (creatures)
           (define next-id (~> (creatures) (sep creature-id) (rectify -1) max add1))
           (define selection
             (~> (mg) monster-group-monsters
                 (and (not empty?) (~> first monster-number))))
           (define c (creature next-id (monster-group* selection mg)))
           (append creatures (list c))))]
    [`(remove ,mg)
     ;; update creatures first: see above comment.
     (<@ (state-@creatures s) {(remf (creature-is-mg~? mg) _)})
     (define set (monster-group-set-name mg))
     ;; valid: typically called from a dialog closer
     (define active-sets
       (~>> (s) state-@active-monster-groups @!
            (map monster-group-set-name)
            list->set))
     ;; only remove ability-decks when no one is using that set
     (unless (set-member? active-sets set)
       (<@ (state-@ability-decks s) {(hash-remove set)}))]))

(module+ test
  (test-case "Add/Remove Monsters: Removing duplicate sets does not remove all ability cards"
    (define s (make-sample-state))
    (define initial-deck (get-ability-decks s archers))
    (define new-mg
      (make-monster-group
       (~> (more-monsters) get-dbs 1>
           (hash-ref "archer") (hash-ref "wyrmling archer"))
       0
       '([5 . #f])
       (hash)))
    ;; add and then remove extra archers
    ((add-or-remove-monster-group s) `(add ,new-mg))
    ((add-or-remove-monster-group s) `(remove ,new-mg))
    (check-equal? (get-ability-decks s archers) initial-deck)))

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
  (<@ (state-@prompts s) {(cons p _)}))

(define ((remove-prompt s) i p)
  (define-values (new-ps p2)
    (list-remove (@! (state-@prompts s)) i))
  (when (equal? p p2)
    (:= (state-@prompts s) new-ps)))
