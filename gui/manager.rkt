#lang racket

(module+ main
  ;; (require racket/gui/easy/debugger)
  ;; (start-debugger)
  (define s (make-state))
  (command-line
    #:args ([save #f])
    (when (and save (file-exists? save))
      ((load-game s) save))
    (void (render/eventspace
            ;; no separate eventspace: block main until this window closed
            (manager s)))))

(provide manager)

(require racket/gui/easy
         (only-in racket/gui
                  get-file
                  put-file
                  application-about-handler)
         frosthaven-manager/observable-operator
         frosthaven-manager/qi
         frosthaven-manager/defns
         frosthaven-manager/manager
         frosthaven-manager/gui/common-menu
         frosthaven-manager/gui/formula-editor
         frosthaven-manager/gui/start
         frosthaven-manager/gui/player-info
         frosthaven-manager/gui/level-info
         frosthaven-manager/gui/loot
         (only-in frosthaven-manager/elements elements)
         frosthaven-manager/gui/elements
         frosthaven-manager/monster-db
         frosthaven-manager/gui/monsters
         frosthaven-manager/gui/render
         frosthaven-manager/gui/rewards
         frosthaven-manager/gui/round-prompts)

(define (manager s)
  (define @undo (make-undo s))
  (application-about-handler do-about)
  (window
    #:title "Frosthaven Manager"
    #:size '(800 600)
    (menu-bar
      (menu "File"
            (menu-item "&Save Game" (thunk (do-save-game s)))
            (menu-item "L&oad Game" (thunk (do-load-game s)))
            (launch-server-menu-item s)
            (formula-menu-item (state-@env s)))
      (menu "Edit"
            (add-prompt-menu-item (λ (p)
                                    (when p
                                      (<~@ (state-@prompts s) (cons p _))))))
      (menu "Help"
            (about-menu-item)
            (how-to-play-menu-item)
            (menu-item-separator)
            (send-feedback-menu-item)
            (issue-menu-item)
            (feature-menu-item)
            (contribute-menu-item)))
    (case-view (state-@mode s)
      [(start) (the-start-view s)]
      [(input-player-info) (input-player-info-view s)]
      [(build-loot-deck) (build-loot-deck-view s)]
      [(choose-monster-db) (choose-monster-db-view s)]
      [(choose-monsters) (choose-monsters-view s)]
      [(play) (play-view s @undo)]
      [else (text "Broken")])))

;;;; GUI

(define (the-start-view s)
  (vpanel
    (start-view #:on-level (λ:= (state-@level s))
                #:on-player (λ:= (state-@num-players s)))
    (button "Play" (to-input-player-info s))))

(define (input-player-info-view s)
  (vpanel
    (player-input-views (state-@num-players s)
                        #:on-name (update-player-name s)
                        #:on-hp (update-player-max-hp s))
    (button "Next" (to-build-loot-deck s))))

(define (build-loot-deck-view s)
  (vpanel
    (loot-picker #:on-card (update-loot-deck-and-num-loot-cards s)
                 #:on-sticker (update-stickers-per-deck s))
    (spacer)
    (button "Next" (to-choose-monster-db s))))

(define (choose-monster-db-view s)
  (define/obs @error-text "")
  (define (call-with-error-text th)
    (:= @error-text "")
    (with-handlers ([exn:fail? (λ (e) (:= @error-text (exn-message e)))])
      (th)))
  (define-syntax-rule (with-error-text e ...)
    (call-with-error-text (thunk e ...)))
  (vpanel
    (db-view (state-@info-db s)
             (state-@ability-db s)
             (@~> (state-@creatures s) (~> sep (pass creature-is-mg*?)
                                           (>< (~> creature-v monster-group*-mg)) collect)))
    (vpanel #:stretch '(#f #f)
            (hpanel #:stretch '(#t #f)
                    (button "Open Bestiary or Foes"
                            (thunk
                              (with-error-text
                                (init-dbs-and-foes
                                  (or (get-file/filter "Bestiary or Foes" '("Bestiary" "*.rkt")) default-monster-db)
                                  s))))
                    (button "Use Default Bestiary"
                            (thunk (with-error-text (init-dbs default-monster-db s)))))
            (cond-view
              [(@> @error-text non-empty-string?)
               (hpanel (text "Error message:" #:color "red")
                       (input @error-text #:style '(multiple)))]
              [else (spacer)]))
    (button "Next" (to-choose-monsters-or-play s) #:enabled? (@~> (state-@info-db s) (not hash-empty?)))))

(define (choose-monsters-view s)
  (vpanel
    (multi-monster-picker (state-@info-db s)
                          (state-@level s)
                          (state-@env s)
                          #:on-change (add-or-remove-monster-group s))
    (button "Next" (to-play s))))

(define (play-view s @undo)
  (vpanel
    (hpanel
      ;; left
      (let* ([es (elements)]
             [@elements (state-@elements s)])
        (vpanel
          #:stretch '(#f #f)
          (elements-cycler @elements es vpanel)
          (button "Infuse All" (thunk (infuse-all @elements)))
          (button "Consume All" (thunk (consume-all @elements)))))
      ;; main
      (group
        "Creatures"
        (list-view
          (obs-combine (λ (cs ads) (sort cs < #:key (creature-initiative ads)))
                       (state-@creatures s) (state-@ability-decks s))
          #:key creature-id
          (make-creature-view s)
          #:style '(vertical vscroll))
        (button "Add Monster Group"
                (thunk
                  (define @monster-names
                    (@~> (state-@creatures s)
                         (~> sep (pass creature-is-mg*?)
                             (>< (~> creature-v monster-group*-mg monster-group-name)) collect)))
                  (add-monster-group
                    (state-@info-db s)
                    (state-@level s)
                    @monster-names
                    (state-@env s)
                    #:on-group
                    (λ (g)
                      ((add-or-remove-monster-group s) `(add ,g))
                      (draw-new-card-mid-round-if-needed s (monster-group-set-name g)))))))
      ;; right
      (vpanel
        #:stretch '(#f #t)
        (deck-adder-button (state-@curses s) (do-curse-monster s) "Curse Monster" monster-curse-deck)
        (deck-adder-button (state-@blesses s) (do-bless-monster s) "Bless Monster" bless-deck)
        (deck-adder-button (state-@blesses s) (do-bless-player s) "Bless Player" bless-deck)
        (deck-adder-button (state-@player-blesses s) (do-unbless-player s) "Unbless Player" bless-deck)
        (spacer)
        (button (@~> (state-@monster-modifier-deck s) (~>> length (format "Draw Modifier (~a)")))
                (draw-modifier s))
        (button "Advantage" (draw-modifier* s))
        (button "Disadvantage" (draw-modifier* s worse-modifier))
        (text (@~> (state-@modifier s) (~>> (or _ "") (~a "Most Recent Modifier: "))))
        (text (@~> (state-@monster-prev-discard s) (~>> (or _ "") (~a "Previous Modifier: "))))
        (button "Show Discard Pile" (show-discard-pile s))
        (spacer)
        (text (@~> (state-@round s) (~a "Round: " _)))
        (button "Next Round" #:enabled? (state-@in-draw? s) (next-round s))
        (button "Draw Abilities" #:enabled? (@> (state-@in-draw? s) not) (draw-abilities s))
        (button "Undo" #:enabled? (@~> @undo undoable?) (thunk (undo! s @undo)))))
    ;; bottom
    (hpanel #:stretch '(#f #f)
            (show-loot-and-xp (state-@num-players s)
                              (state-@level s)
                              (@~> (state-@creatures s)
                                   (~> sep (pass (~> creature-v player?)) (>< creature-v) collect)))
            (loot-button (state-@loot-deck s)
                         (state-@num-loot-cards s)
                         (state-@num-players s)
                         (@~> (state-@creatures s)
                              (~> sep (pass (~> creature-v player?))
                                  (>< (~> (-< creature-v creature-id) cons)) collect))
                         #:on-player (give-player-loot s)
                         ;; #:on-top: do nothing :)
                         #:on-bottom (thunk (place-loot-on-bottom s)))
            (loot-preview (state-@loot-deck s) (state-@num-players s))
            (level-stats (state-@level s) (state-@num-players s))
            (level-table (state-@level s))
            (inspiration-table (state-@num-players s)))))

(define (deck-adder-button @cards do-adder text original-deck)
  (button
    #:enabled? (@~> @cards (not empty?))
    (@~> @cards
         (~> length
             (format "~a (~a/~a)" text _ (length original-deck))))
    do-adder))

(define ((make-player-view s) k @e)
  (define (update proc)
    (<~@ (state-@creatures s) (update-players k proc)))
  (define-flow update-player-condition (~> player-condition-handler update))
  (define-flow update-player-hp (~> player-act-on-hp update))
  (define-flow update-player-xp (~> player-act-on-xp update))
  (define (update-player-initiative i)
    (update (flow (player-set-initiative i))))
  (define (update-summon-hp i proc)
    (update (update-player-summon i (summon-act-on-hp proc))))
  (define (update-summon-condition i c)
    (update (update-player-summon i (summon-condition-handler c))))
  (player-view
    (@> @e creature-v)
    #:on-condition update-player-condition
    #:on-hp update-player-hp
    #:on-xp update-player-xp
    #:on-initiative update-player-initiative
    #:on-summon (flow (~>> (clos player-summon) update))
    #:kill-summon (flow (~> player-kill-summon update))
    #:on-summon-hp update-summon-hp
    #:on-summon-condition update-summon-condition))

(define ((make-monster-group-view s) k @e)
  (define @env (state-@env s))
  (define (update proc [procn (flow 1>)])
    (<~@ (state-@creatures s) (update-monster-groups k proc procn)))
  (define (update-by-num num proc)
    (update (monster-group-update-num num proc)))
  (define @mg* (@> @e creature-v))
  (define @mg (@> @mg* monster-group*-mg))
  (define @n (@> @mg* monster-group*-active))
  (define (update-condition num c on?)
    (update-by-num num (monster-update-condition c on?)))
  (define (update-hp num proc)
    (update-by-num num (monster-update-hp proc)))
  (define (kill num)
    (update (monster-group-remove num)
            (flow (~> 2> monster-group-first-monster))))
  (define (new num elite?)
    (update (monster-group-add num elite? (@! @env))
            (const num))
    ;; Probably this could be done unconditionally, since
    ;; draw-new-card-mid-round-if-needed checks that there isn't already a card
    ;; flipped. But this also probably doesn't hurt? Unless length becomes a
    ;; perf. issue, which is unlikely.
    (when (@! (@~> @mg (~> monster-group-monsters length (= 1))))
      (draw-new-card-mid-round-if-needed s (@! (@> @mg monster-group-set-name)))))
  (define (select num) (update values (const num)))
  (define/match (swap who)
    [{'all} (update swap-monster-group-elites)]
    [{n} (update-by-num n swap-monster-elite)])
  (define @ability-deck
    (obs-combine
      (flow (~> (== _ monster-group-set-name) hash-ref))
      (state-@ability-decks s) @mg))
  (define (move-ability-card)
    (<~@ (state-@ability-decks s)
         ;; valid: in a dialog handler
         (hash-update (monster-group-set-name (@! @mg))
                      move-top-draw-to-bottom)))
  (monster-group-view
    @mg
    @ability-deck
    @n
    @env
    #:on-condition update-condition
    #:on-hp update-hp
    #:on-kill kill
    #:on-new new
    #:on-select select
    #:on-swap swap
    #:on-move-ability-card move-ability-card))

(define ((make-creature-view s) k @e)
  (cond-view
    [(@~> @e (~> creature-v player?)) ((make-player-view s) k @e)]
    [(@~> @e (~> creature-v monster-group*?)) ((make-monster-group-view s) k @e)]
    [else (text "creature is neither player or monster-group*")]))

(define ((show-discard-pile s))
  (with-closing-custodian/eventspace
    (render/eventspace
      #:eventspace closing-eventspace
      (window
        #:mixin close-custodian-mixin
        #:title "Discard Pile"
        #:min-size (@~> (state-@monster-discard s) (~>> length (* 20) (list 200)))
        (text "(Most recent first)")
        (spacer)
        (text (@~> (state-@monster-discard s) (~>> (map ~a) (string-join _ "\n"))))
        (spacer)))))

(define (show-loot-and-xp @num-players @level @players)
  (button
   "Show Loot and XP"
   (thunk
    (with-closing-custodian/eventspace
     (render/eventspace
      #:eventspace closing-eventspace
      (player-rewards-view @num-players @level @players #:mixin close-custodian-mixin))))))

;;;; Transition functions

(define ((to-input-player-info s))
  (when (empty? (@! (state-@creatures s)))
    (:= (state-@creatures s) (build-list (@! (state-@num-players s)) make-player-creature)))
  (:= (state-@mode s) 'input-player-info))

(define ((to-build-loot-deck s))
  ;; give each player max-hp
  (<~@ (state-@creatures s)
        (update-all-players
          (flow (~> (-< (~> player-max-hp const player-act-on-hp) _) apply))))
  (:= (state-@mode s) 'build-loot-deck))

(define ((to-play s))
  (:= (state-@mode s) 'play))

(define ((to-choose-monster-db s))
  (build-loot-deck! s)
  (:= (state-@mode s) 'choose-monster-db))
(define ((to-choose-monsters s))
  (:= (state-@mode s) 'choose-monsters))

(define-flow creature-is-mg*? (~> creature-v monster-group*?))
(define ((to-choose-monsters-or-play s))
  (define-flow has-mg*? (~>> state-@creatures @! (memf creature-is-mg*?)))
  ;; note parens around switch to invoke selected transition function
  ((switch (s)
     [has-mg*? to-play]
     [else to-choose-monsters])))

(define ((next-round s))
  ;; check prompts
  (let ([t end-of]
        [round (@! (state-@round s))])
    (when (should-do-prompt? t round (@! (state-@prompts s)))
      (do-round-prompt t round)))
  ;; wane elements
  (for-each (flow (<@ wane-element)) (state-@elements s))
  ;; reset player initiative
  (<~@ (state-@creatures s) (update-all-players player-clear-initiative))
  ;; discard monster cards
  (<@ (state-@ability-decks s)
      (update-ability-decks ability-decks-discard-and-maybe-shuffle))
  ;; shuffle modifiers if required
  (when (shuffle-modifier-deck? (@! (state-@monster-discard s)))
    (reshuffle-modifier-deck s))
  ;; increment round number
  (<@ (state-@round s) add1)
  ;; toggle state
  (<@ (state-@in-draw? s) not)
  ;; check prompts
  (let ([t beginning-of]
        [round (@! (state-@round s))])
    (when (should-do-prompt? t round (@! (state-@prompts s)))
      (do-round-prompt t round))))

(define ((draw-abilities s))
  ;; draw new monster cards
  (<@ (state-@ability-decks s)
      (update-ability-decks
        (λ (ad)
          (define monster-set
            (for/or ([ability (cons (ability-decks-current ad)
                                    (append (ability-decks-draw ad)
                                            (ability-decks-discard ad)))])
              (and ability (monster-ability-set-name ability))))
          (define monster-set-has-monsters?
            (for/or ([creature (@! (state-@creatures s))]
                     #:do [(define v (creature-v creature))]
                     #:when (monster-group*? v)
                     #:do [(define mg (monster-group*-mg v))]
                     #:when (~> (mg) monster-group-set-name (equal? monster-set)))
              (~> (mg) monster-group-monsters (not empty?))))
          (cond
            [monster-set-has-monsters? (ability-decks-draw-next ad)]
            [else ad]))))
  ;; toggle state
  (<@ (state-@in-draw? s) not))

;;;; Save & Load

(define ((save-game s) p)
  (call-with-output-file* p (curry serialize-state s) #:exists 'replace))

(define (do-save-game s)
  (cond [(put-file/filter "Save Game" '("Saved Games" "*.fasl")) => (save-game s)]))

(define ((load-game s) p)
  (define saved-state (call-with-input-file* p deserialize-state))
  (copy-state saved-state s))

(define (do-load-game s)
  (cond [(get-file/filter "Load Game" '("Saved Games" "*.fasl")) => (load-game s)]))

;;;; Files

(define (get-file/filter message filter)
  (get-file message #f #f #f (->extension (second filter)) empty (list filter '("Any" "*.*"))))

(define (put-file/filter message filter)
  (put-file message #f #f #f (->extension (second filter)) empty (list filter '("Any" "*.*"))))

(define-flow ->extension
  (~> path-get-extension (and _ (~> bytes->string/utf-8 (substring 1)))))
