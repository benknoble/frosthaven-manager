#lang racket

(module+ main
  (require racket/gui/easy/debugger
           (prefix-in dbg: debugging/server)
           (only-in racket/gui
                    event-dispatch-handler))
  (define base-event-handler (event-dispatch-handler))
  (event-dispatch-handler
   (λ (es)
     ;; Request incremental GC until the next major GC. Since we do this in the
     ;; GUI event loop, it should be enabled for the entire life of the
     ;; application.
     (collect-garbage 'incremental)
     (base-event-handler es)))
  (define s (make-state))
  (unless (terminal-port? (current-error-port))
    (define temp (make-temporary-file "frosthaven-manager-~a"))
    (current-error-port (open-output-file temp #:exists 'truncate #:mode 'text))
    (:= (state-@error-logs s) temp))
  (command-line
   #:once-each
   [("--debug") "Enable debugging"
                (start-debugger)
                (dbg:serve)]
   #:args ([save #f])
   (when (and save (file-exists? save))
     ((load-game s) save))
   (void (render/eventspace
          ;; no separate eventspace: block main until this window closed
          (manager s)))))

(provide manager)

(require frosthaven-manager/defns
         frosthaven-manager/files
         frosthaven-manager/gui/common-menu
         frosthaven-manager/gui/elements
         frosthaven-manager/gui/formula-editor
         frosthaven-manager/gui/helpers
         frosthaven-manager/gui/level-info
         frosthaven-manager/gui/level-picker
         frosthaven-manager/gui/loot
         frosthaven-manager/gui/mixins
         frosthaven-manager/gui/monster-modifier
         frosthaven-manager/gui/monsters
         frosthaven-manager/gui/number-players
         frosthaven-manager/gui/player-info
         frosthaven-manager/gui/render
         frosthaven-manager/gui/rewards
         frosthaven-manager/gui/rich-text-display
         frosthaven-manager/gui/round-number
         frosthaven-manager/gui/round-prompts
         frosthaven-manager/manager
         frosthaven-manager/monster-db
         frosthaven-manager/observable-operator
         racket/gui/easy
         (only-in racket/gui get-file put-file application-about-handler)
         (only-in frosthaven-manager/elements elements))

(define modifier
  (case (system-type 'os)
    [(macosx) 'cmd]
    [else 'ctl]))

(define (manager s)
  (application-about-handler do-about)
  (window
    #:title "Frosthaven Manager"
    (menu-bar
      (menu "File"
            (menu-item "&Save Game" (thunk (do-save-game s))
                       #:shortcut (list modifier #\s))
            (menu-item "L&oad Game" (thunk (do-load-game s))
                       #:shortcut (list modifier #\o))
            (launch-server-menu-item s))
      (menu "Edit"
            (edit-level-menu-item (state-@level s)
                                  (λ:= (state-@level s)))
            (edit-players-menu-item s)
            (edit-bestiary-menu-item s)
            (edit-loot-deck-menu-item s)
            (add-monster-group-menu-item s)
            (edit-round-number-menu-item s)
            (manage-prompt-menu-item (state-@prompts s)
                                     #:on-add (add-prompt s)
                                     #:on-remove (remove-prompt s))
            (modify-monster-deck-menu-item (obs-combine append
                                                        (state-@monster-modifier-deck s)
                                                        (state-@monster-discard s))
                                           #:on-add (add-monster-modifier s)
                                           #:on-remove (remove-monster-modifier s)
                                           #:on-shuffle (thunk (reshuffle-modifier-deck s))))
      (menu "Utilities"
            (formula-menu-item (state-@env s)))
      (menu "Debug"
            (error-logs-menu-item (state-@error-logs s))
            (gc-menu-item))
      (menu "Help"
            (about-menu-item)
            (how-to-play-menu-item)
            (menu-item-separator)
            (send-feedback-menu-item)
            (issue-menu-item)
            (feature-menu-item)
            (contribute-menu-item)))
    (play-view s)))

;;;; GUI

(define (play-view s)
  (define undo (make-undo s))
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
        (button "Add Monster Group" (thunk (do-add-monster-group s))))
      ;; right
      (vpanel
        #:stretch '(#f #t)
        (deck-adder-button (state-@curses s) (do-curse-monster s) "Curse Monster" monster-curse-deck)
        (deck-adder-button (state-@blesses s) (do-bless-monster s) "Bless Monster" bless-deck)
        (deck-adder-button (state-@blesses s) (do-bless-player s) "Bless Player" bless-deck)
        (deck-adder-button (state-@player-blesses s) (do-unbless-player s) "Unbless Player" bless-deck)
        (spacer)
        (button (@> (state-@monster-modifier-deck s) {~>> length (format "Draw Modifier (~a)")})
                (draw-modifier s))
        (button "Advantage" (draw-modifier* s))
        (button "Disadvantage" (draw-modifier* s worse-modifier))
        (button (@> (state-@modifier s)
                    {~>> (if _ format-monster-modifier "N/A")
                         (format "Show Discard (top: ~a)")})
                (show-discard-pile s))
        (spacer)
        (text (@> (state-@round s) {(~a "Round: " _)}))
        (button "Next Round" #:enabled? (state-@in-draw? s) (next-round s))
        (button "Draw Abilities" #:enabled? (@> (state-@in-draw? s) not) (draw-abilities s))
        (button "Undo" #:enabled? (undoable? undo) (thunk (undo! s undo)))))
    ;; bottom
    (hpanel #:stretch '(#f #f)
            (show-loot-and-xp (state-@num-players s)
                              (state-@level s)
                              (@> (state-@creatures s)
                                  {~> sep (pass (~> creature-v player?)) (>< creature-v) collect}))
            (loot-button (state-@loot-deck s)
                         (state-@num-loot-cards s)
                         (state-@num-players s)
                         (@> (state-@creatures s)
                             {~> sep (pass (~> creature-v player?))
                                 (>< (~> (-< creature-v creature-id) cons)) collect})
                         #:on-player (give-player-loot s)
                         ;; #:on-top: do nothing :)
                         #:on-bottom (thunk (place-loot-on-bottom s)))
            (loot-preview (state-@loot-deck s) (state-@num-players s))
            (level-stats (state-@level s) (state-@num-players s))
            (level-table (state-@level s))
            (inspiration-table (state-@num-players s)))))

(define (deck-adder-button @cards do-adder text original-deck)
  (button
   #:enabled? (@> @cards {(not empty?)})
   (@> @cards
       {~> length
           (format "~a (~a/~a)" text _ (length original-deck))})
   do-adder))

(define ((make-player-view s) k @e)
  (define (update proc)
    (<@ (state-@creatures s) {(update-players k proc)}))
  (define update-player-condition {~> player-condition-handler update})
  (define update-player-hp {~> player-act-on-hp update})
  (define update-player-xp {~> player-act-on-xp update})
  (define (update-player-initiative i)
    (update {(player-set-initiative i)}))
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
    #:on-update update
    #:on-summon {~>> (clos player-summon) update}
    #:kill-summon {~> player-kill-summon update}
    #:on-summon-hp update-summon-hp
    #:on-summon-condition update-summon-condition))

(define ((make-monster-group-view s) k @e)
  (define @env (state-@env s))
  (define (update proc [procn {1>}])
    (<@ (state-@creatures s) {(update-monster-groups k proc procn)}))
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
    (kill-monster s k num))
  (define (new num elite?)
    (update (monster-group-add num elite? (@! @env))
            (const num))
    ;; Probably this could be done unconditionally, since
    ;; draw-new-card-mid-round-if-needed checks that there isn't already a card
    ;; flipped. But this also probably doesn't hurt?
    (when (@! (@> @mg {~> monster-group-monsters cdr empty?}))
      (draw-new-card-mid-round-if-needed s (@! (@> @mg monster-group-set-name)))))
  (define (select num) (update values (const num)))
  (define/match (swap _who)
    [{'all} (update swap-monster-group-elites)]
    [{n} (update-by-num n swap-monster-elite)])
  (define @ability-deck
    (obs-combine
      {~> (== _ monster-group-set-name)
          (hash-ref (ability-decks #f empty empty))}
      (state-@ability-decks s) @mg))
  (define (move-ability-card)
    (<@ (state-@ability-decks s)
        ;; valid: in a dialog handler
        {(hash-update (monster-group-set-name (@! @mg))
                      move-top-draw-to-bottom)}))
  (define (update-max-hp f)
    (update {(monster-group-change-max-HP f (@! @env))}))
  (define (update-level new-level)
    (update
     (λ (mg)
       (define info (~> (s) state-@info-db @!
                        (hash-ref (monster-group-set-name mg))
                        (hash-ref (monster-group-name mg))))
       (monster-group-update-level mg info new-level))))
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
    #:on-move-ability-card move-ability-card
    #:on-max-hp update-max-hp
    #:on-change-level update-level
    #:on-update update))

(define ((make-creature-view s) k @e)
  (cond-view
    [(@> @e {~> creature-v player?}) ((make-player-view s) k @e)]
    [(@> @e creature-is-mg*?) ((make-monster-group-view s) k @e)]
    [else (text "creature is neither player or monster-group*")]))

(define ((show-discard-pile s))
  (with-closing-custodian/eventspace
    (render/eventspace
      #:eventspace closing-eventspace
      (window
       #:mixin close-custodian-mixin
       #:title "Discard Pile"
       #:min-size '(200 300)
       (table
        '("Ability Modifier (Most recent first)")
        (@> (state-@monster-discard s) list->vector)
        #:entry->row {~> format-monster-modifier vector})))))

(define (show-loot-and-xp @num-players @level @players)
  (button
   "Show Loot and XP"
   (thunk
    (with-closing-custodian/eventspace
     (render/eventspace
      #:eventspace closing-eventspace
      (player-rewards-view @num-players @level @players #:mixin close-custodian-mixin))))))

(define (db-picker* s)
  (define-error-text @error-text with-error-text)
  (list
   (db-view (state-@info-db s)
            (state-@ability-db s)
            (@> (state-@creatures s) {~> sep (pass creature-is-mg*?)
                                         (>< (~> creature-v monster-group*-mg)) collect}))
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
                      (rich-text-display (@> @error-text {~> (string-split "\n") (add-between newline)})
                                         #:min-size '(#f 60)))]
             [else (spacer)]))))

;;;; Menu Items

(define (edit-level-menu-item @level [on-change void])
  (menu-item
   "Edit Scenario Level"
   (thunk
    (define-close! close! closing-mixin)
    ;; not setting current renderer, nor using an eventspace: dialog
    (render
     (dialog
      #:mixin closing-mixin
      #:title "Change Scenario Level"
      #:style '()
      (hpanel
       (level-picker #:choose on-change #:selection @level #:label "Scenario Level")
       (button "Ok" close!)))))))

(define (add-monster-group-menu-item s)
  (menu-item
   "Add Monster Group"
   (thunk (do-add-monster-group s))
   #:shortcut (list modifier #\n)))

(define (do-add-monster-group s)
  (unless (@! (state-@bestiary-path s))
    ;; not setting current renderer, nor using an eventspace: dialog
    (render (get-bestiary-dialog s)))
  (define @monster-names
    (@> (state-@creatures s)
        {~> sep (pass creature-is-mg*?)
            (>< (~> creature-v monster-group*-mg monster-group-name)) collect}))
  (add-monster-group
   (state-@info-db s)
   (state-@level s)
   @monster-names
   (state-@env s)
   #:on-group
   (λ (g)
     ((add-or-remove-monster-group s) `(add ,g))
     (draw-new-card-mid-round-if-needed s (monster-group-set-name g)))))

(define (edit-players-menu-item s)
  (menu-item
   "Edit Number of Players"
   (thunk
    (define-close! close! closing-mixin)
    (define/obs @num-players (@! (state-@num-players s)))
    (define (finish!)
      (:= (state-@num-players s) (@! @num-players))
      (setup-players s)
      (close!))
    ;; not setting current renderer, nor using an eventspace: dialog
    (render
     (dialog
      #:mixin closing-mixin
      #:title "Change Number of Players"
      #:style '()
      (number-players-picker #:choose (λ:= @num-players) #:selection @num-players)
      (hpanel
       (button "Ok" finish!)
       (button "Cancel" close!)))))))

(define (edit-round-number-menu-item s)
  (menu-item
   "Edit round number"
   (thunk
    ;; not setting current renderer, nor using an eventspace: dialog
    (render
     (round-number-modifier
      (state-@round s)
      #:new-round-number (λ (p) (<@ (state-@round s) p)))))))

(define (edit-bestiary-menu-item s)
  (menu-item
   "Choose Bestiary"
   (thunk
    ;; not setting current renderer, nor using an eventspace: dialog
    (render (get-bestiary-dialog s)))))

(define (get-bestiary-dialog s)
  (define-close! close! closing-mixin)
  (apply
   dialog
   #:mixin closing-mixin
   #:title "Choose Bestiary"
   #:size '(600 400)
   (append
    (db-picker* s)
    (list
     (button "Ok" close!)))))

(define (edit-loot-deck-menu-item s)
  (define (get-dialog)
    (define-close! close! closing-mixin)
    (define (finish!)
      (build-loot-deck! s)
      (close!))
    (dialog
     #:mixin closing-mixin
     #:title "Build Loot Deck"
     #:style '() ;; no close button: need to finish! to build the loot deck
     (loot-picker (state-@type->number-of-cards s)
                  (state-@type->deck s)
                  #:on-card (update-loot-deck-and-num-loot-cards s)
                  #:on-deck (λ:= (state-@type->deck s)))
     (button "Ok" finish!)))
  (menu-item
   "Build Loot Deck"
   (thunk
    ;; not setting current renderer, nor using an eventspace: dialog
    (render (get-dialog)))))
