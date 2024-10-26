#lang racket

(provide
  (contract-out
    [player-view (->* ((obs/c player?))
                      (#:on-condition (-> (list/c condition? boolean?) any)
                       #:on-hp (-> (-> number? number?) any)
                       #:on-xp (-> (-> number? number?) any)
                       #:on-initiative (-> number? any)
                       #:on-update (-> (-> player? player?) any)
                       #:on-summon (-> string? positive-integer? any)
                       #:on-summon-hp (-> natural-number/c (-> number? number?) any)
                       #:on-summon-condition (-> natural-number/c (list/c condition? boolean?) any)
                       #:kill-summon (-> natural-number/c any))
                      (is-a?/c view<%>))]))

(require frosthaven-manager/defns
         frosthaven-manager/gui/counter
         frosthaven-manager/gui/font
         frosthaven-manager/gui/helpers
         frosthaven-manager/gui/mixins
         frosthaven-manager/gui/render
         frosthaven-manager/gui/rich-text-display
         frosthaven-manager/observable-operator
         racket/gui/easy
         racket/gui/easy/contract)

(define (player-view @player
                     #:on-condition [on-condition void]
                     #:on-hp [on-hp void]
                     #:on-xp [on-xp void]
                     #:on-initiative [on-initiative void]
                     #:on-update [arbitrary-update void]
                     #:on-summon [add-summon void]
                     #:on-summon-hp [on-summon-hp void]
                     #:on-summon-condition [on-summon-condition void]
                     #:kill-summon [kill-summon void])
  (define (make-condition-checkbox c)
    (checkbox #:label (format-condition c)
              #:checked? (@> @player (player-afflicted-by? c))
              {~>> (list c) on-condition}))
  (define (subtract-hp)
    (unless (@! (@> @player player-dead?))
      (on-hp sub1)))
  (define (add-hp)
    (unless (@! (@> @player player-at-max-health?))
      (on-hp add1)))
  (define hp-panel
    (counter (@> @player player->hp-text) add-hp subtract-hp))
  (define (subtract-xp)
    (unless (@! (@> @player {~> player-xp zero?}))
      (on-xp sub1)))
  (define (add-xp)
    (on-xp add1))
  (define xp-panel
    (counter (@> @player {~>> player-xp (~a "XP: ")}) add-xp subtract-xp))
  (define hp-xp
    (vpanel
      #:alignment '(center center)
      #:stretch '(#f #t)
      hp-panel xp-panel))
  (define @init (@> @player player-initiative))
  (define @init-label (@> @player {~>> player-name (~a "Initiative for ")}))
  (define (input-initiative)
    (define-close! close! closing-mixin)
    (with-closing-custodian/eventspace
      (render/eventspace
        #:eventspace closing-eventspace
        (window
          #:mixin {~> close-custodian-mixin closing-mixin}
          #:title @init-label
          (input
            (@> @init ~a)
            (λ (event init-str)
              (when (equal? event 'return)
                (close!))
              (cond [(string->number init-str) => on-initiative]))
            #:label (@> @init-label escape-text))))))
  (define name-initiative-panel
    (vpanel
      #:stretch '(#f #t)
      (hpanel #:alignment '(center center)
              (text (@> @player {~> player-name escape-text}) #:font big-control-font)
              (text (@> @init {(format "(~a)" _)})))
      (button "Edit Initiative" input-initiative)))
  (define (show-conditions)
    (with-closing-custodian/eventspace
      (render/eventspace
        #:eventspace closing-eventspace
        (apply window
               #:mixin close-custodian-mixin
               #:title (@> @player {~>> player-name (~a "Conditions for ")})
               #:size '(200 #f)
               (map make-condition-checkbox conditions)))))
  (define (expire-conditions)
    (arbitrary-update player-expire-conditions))
  (define conditions-panel
    (vpanel
      (rich-text-display (@> @player {~> player-conditions* conditions->string list})
                         #:min-size '(50 30))
      (hpanel
       (button "Edit Conditions" show-conditions)
       (button "Expire Conditions" expire-conditions))))
  (define (change-max-hp)
    (define-close! close! closing-mixin)
    ;; valid: inside a dialog
    (define/obs @new-max-hp (player-max-hp (@! @player)))
    (define (change!)
      (define new-max-hp (@! @new-max-hp))
      (arbitrary-update (player-act-on-max-hp (const new-max-hp)))
      (close!))
    ;; not setting current renderer, nor using an eventspace: dialog
    (render
     (dialog
      #:min-size '(400 #f)
      #:mixin closing-mixin
      #:title (@> @player {~>> player-name escape-text (~a "New maximum HP for ")})
      #:style '()
      (counter (@> @new-max-hp {(~a "New maximum HP: " _)})
               {(<@ @new-max-hp add1)}
               {(<@ @new-max-hp sub1)})
      (hpanel
       (button "Change" change!)
       (button "Cancel" close!)))))
  (define (add-summon-button)
    (button "Summon" (thunk (do-summon add-summon))))
  (define ((summon-condition i) evt)
    (on-summon-condition i evt))
  (define ((summon-add-hp i s))
    (unless (summon-at-max-health? s)
      (on-summon-hp i add1)))
  (define ((summon-sub-hp i s))
    (unless (summon-dead? s)
      (on-summon-hp i sub1)))
  (define (summons-view)
    (cond-view
      [(@> @player {~> player-summons (not empty?)})
       (group
        "Summons"
        (observable-view
         (@> @player player-summons)
         (λ (summons)
           (apply vpanel
                  (for/list ([(s i) (in-indexed (in-list summons))])
                    (summon-view s
                                 (thunk (kill-summon i))
                                 (summon-add-hp i s)
                                 (summon-sub-hp i s)
                                 (summon-condition i)))))))]
      [else (spacer)]))
  (define (more-actions)
    (button "More Actions…"
            (thunk
             ;; not setting current renderer, nor using an eventspace: dialog
             (render
              (dialog
               #:min-size '(400 #f)
               #:title (@> @player {~>> player-name escape-text (~a "More Actions for ")})
               (add-summon-button)
               (button "Change Max. HP" change-max-hp)
               (button "Edit Name" (thunk (edit-name (player-name (@! @player))
                                                     arbitrary-update))))))))
  ;; final view
  (group
    "Player"
    #:stretch '(#t #f)
    (hpanel #:alignment '(center center)
            #:margin '(20 0)
            name-initiative-panel
            hp-xp
            (more-actions)
            conditions-panel)
    (summons-view)))

(define (summon-view s die add-hp subtract-hp on-condition)
  (define (make-condition-checkbox c)
    (checkbox #:label (format-condition c)
              #:checked? ((summon-afflicted-by? c) s)
              {~>> (list c) on-condition}))
  (define (edit-conditions)
    (with-closing-custodian/eventspace
     (render/eventspace
      #:eventspace closing-eventspace
      (apply window
             #:mixin close-custodian-mixin
             #:title (~> (s) (~>> summon-name (~a "Conditions for ")))
             #:size '(200 #f)
             (map make-condition-checkbox conditions)))))
  (hpanel
   (button "💀Kill💀" die)
   (text (escape-text (summon-name s)))
   (counter (summon->hp-text s) add-hp subtract-hp)
   (rich-text-display (~> (s) summon-conditions* conditions->string list)
                      #:min-size '(50 30))
   (button "Edit Conditions" edit-conditions)))

(define (do-summon add-summon)
  (define/obs @name "")
  (define/obs @hp 1)
  (define-close! close! closing-mixin)
  (render
   (dialog
    #:title "Summon"
    #:mixin {~> closing-mixin
                (esc (make-on-close-mixin
                      (thunk
                       (add-summon (@! @name) (@! @hp)))))}
    (hpanel (input @name (match-lambda**
                           [{'input s} (:= @name s)]
                           [{'return s} (:= @name s) (close!)]))
            (counter (@> @hp {(~a "Max HP: " _)})
                     (thunk (<@ @hp add1))
                     (thunk (<@ @hp {switch [(<= 1) _] [else sub1]})))
            (button "Summon" close!)))))

(define (edit-name name update)
  (define/obs @name name)
  (define-close! close! closing-mixin)
  (define (finish!)
    (update (player-update-name (@! @name)))
    (close!))
  ;; not setting current renderer, nor using an eventspace: dialog
  (render
   (dialog
    #:title "Edit Player name"
    #:mixin closing-mixin
    #:style '(close-button resize-border)
    (input @name (match-lambda**
                   [{'input s} (:= @name s)]
                   [{'return s} (:= @name s) (finish!)]))
    (hpanel (button "Accept" finish!)
            (button "Cancel" close!)))))

(module+ main
  (define (update-players players k f)
    (for/list ([e (in-list players)])
      (if (eq? (car e) k)
          (cons k (f (cdr e)))
          e)))
  (define/obs @players
    (list
      (cons 0 (player "A" 15 10 3 (list regenerate invisible immobilize) 23
                      (list random-item)
                      empty))
      (cons 1 (player "B" 20 20 0 (list brittle) 57
                      (list (first money-deck)
                            (first (hash-ref material-decks lumber)))
                      empty))
      (cons 2 (player "C" 8 0 5 empty 99 empty empty))))
  (void
    ;; no separate eventspace: block main until this window closed
    (render/eventspace
      (window
       (list-view @players
         #:key car
         #:min-size (@> @players {~>> length (* 100) (list #f)})
         (λ (k @e)
           (define (update proc)
             (<@ @players {(update-players k proc)}))
           (player-view
            (@> @e cdr)
            #:on-update update
            #:on-condition {~> player-condition-handler update}
            #:on-hp {~> player-act-on-hp update}
            #:on-xp {~> player-act-on-xp update}
            #:on-initiative (λ (i) (update {(player-set-initiative i)}))
            #:on-summon
            (λ (name hp)
              (update {(player-summon name hp)}))
            #:on-summon-hp
            (λ (i proc)
              (update
               (update-player-summon i (summon-act-on-hp proc))))
            #:on-summon-condition
            (λ (i c)
              (update
               (update-player-summon i (summon-condition-handler c))))
            #:kill-summon
            (λ (i) (update (player-kill-summon i))))))))))
