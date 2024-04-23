#lang racket

(provide
  (contract-out
    [player-input-views (->* ((obs/c natural-number/c))
                             (#:on-name (-> natural-number/c string? any)
                              #:on-hp (-> natural-number/c
                                          (-> number? number?)
                                          any)
                              #:names (listof string?)
                              #:hps (listof positive-integer?))
                             (is-a?/c view<%>))]
    [player-view (->* ((obs/c player?))
                      (#:on-condition (-> (list/c condition? boolean?) any)
                       #:on-hp (-> (-> number? number?) any)
                       #:on-xp (-> (-> number? number?) any)
                       #:on-initiative (-> number? any)
                       #:on-summon (-> string? positive-integer? any)
                       #:on-summon-hp (-> natural-number/c (-> number? number?) any)
                       #:on-summon-condition (-> natural-number/c (list/c condition? boolean?) any)
                       #:kill-summon (-> natural-number/c any))
                      (is-a?/c view<%>))]))

(require racket/gui/easy
         frosthaven-manager/observable-operator
         racket/gui/easy/contract
         frosthaven-manager/defns
         frosthaven-manager/gui/counter
         frosthaven-manager/gui/render
         frosthaven-manager/gui/mixins
         frosthaven-manager/gui/font
         frosthaven-manager/gui/helpers
         frosthaven-manager/gui/rich-text-display)

(define (player-input-views @num-players
                            #:on-name [on-name void]
                            #:on-hp [on-hp void]
                            #:names [names #f]
                            #:hps [hps #f])
  (define (make-input-view k _@i)
    (define-flow do-name (on-name k _))
    (define-flow do-hp (on-hp k _))
    (player-input-view
      #:on-name do-name
      #:on-hp do-hp
      #:name (if names (list-ref names k) "")
      #:hp (if hps (list-ref hps k) 1)))
  (list-view (@> @num-players range)
    #:min-size (@~> @num-players (~>> (* 40) (list #f)))
    make-input-view))

(define (player-view @player
                     #:on-condition [on-condition void]
                     #:on-hp [on-hp void]
                     #:on-xp [on-xp void]
                     #:on-initiative [on-initiative void]
                     #:on-summon [add-summon void]
                     #:on-summon-hp [on-summon-hp void]
                     #:on-summon-condition [on-summon-condition void]
                     #:kill-summon [kill-summon void])
  (define (make-condition-checkbox c)
    (checkbox #:label (~a c)
              #:checked? (@> @player (player-afflicted-by? c))
              (flow (~>> (list c) on-condition))))
  (define (subtract-hp)
    (unless (@! (@> @player player-dead?))
      (on-hp sub1)))
  (define (add-hp)
    (unless (@! (@> @player player-at-max-health?))
      (on-hp add1)))
  (define hp-panel
    (counter (@> @player player->hp-text) add-hp subtract-hp))
  (define (subtract-xp)
    (unless (@! (@~> @player (~> player-xp zero?)))
      (on-xp sub1)))
  (define (add-xp)
    (on-xp add1))
  (define xp-panel
    (counter (@~> @player (~>> player-xp (~a "XP: "))) add-xp subtract-xp))
  (define hp-xp
    (vpanel
      #:alignment '(center center)
      #:stretch '(#f #t)
      hp-panel xp-panel))
  (define @init (@> @player player-initiative))
  (define @init-label (@~> @player (~>> player-name (~a "Initiative for "))))
  (define (input-initiative)
    (define-close! close! closing-mixin)
    (with-closing-custodian/eventspace
      (render/eventspace
        #:eventspace closing-eventspace
        (window
          #:mixin (flow (~> close-custodian-mixin closing-mixin))
          #:title @init-label
          (input
            (@> @init ~a)
            (λ (event init-str)
              (when (equal? event 'return)
                (close!))
              (cond
                [(string->number init-str)
                 =>
                 (λ (init) (when (<= 0 init 99) (on-initiative init)))]))
            #:label @init-label)))))
  (define name-initiative-panel
    (vpanel
      #:stretch '(#f #t)
      (hpanel #:alignment '(center center)
              (text (@~> @player (~> player-name escape-text)) #:font big-control-font)
              (text (@~> @init (format "(~a)" _))))
      (button "Edit Initiative" input-initiative)))
  (define (show-conditions)
    (with-closing-custodian/eventspace
      (render/eventspace
        #:eventspace closing-eventspace
        (apply window
               #:mixin close-custodian-mixin
               #:title (@~> @player (~>> player-name (~a "Conditions for ")))
               #:size '(200 #f)
               (map make-condition-checkbox conditions)))))
  (define conditions-panel
    (vpanel
      (rich-text-display (@~> @player (~> player-conditions* conditions->string list))
                         #:min-size '(50 30))
      (button "Edit Conditions" show-conditions)))
  (define add-summon-button
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
      [(@~> @player (~> player-summons (not empty?)))
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
  ;; final view
  (group
    "Player"
    #:stretch '(#t #f)
    (hpanel #:alignment '(center center)
            #:margin '(20 0)
            name-initiative-panel
            hp-xp
            add-summon-button
            conditions-panel)
    (summons-view)))

(define (summon-view s die add-hp subtract-hp on-condition)
  (define (make-condition-checkbox c)
    (checkbox #:label (~a c)
              #:checked? ((summon-afflicted-by? c) s)
              (flow (~>> (list c) on-condition))))
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
    #:mixin (flow (~> closing-mixin
                      (esc (make-on-close-mixin
                            (thunk
                             (add-summon (@! @name) (@! @hp)))))))
    (hpanel (input @name (match-lambda**
                           [{'input s} (:= @name s)]
                           [{'return s} (:= @name s) (close!)]))
            (counter (@~> @hp (~a "Max HP: " _))
                     (thunk (<@ @hp add1))
                     (thunk (<~@ @hp (switch [(<= 1) _] [else sub1]))))
            (button "Summon" close!)))))

(define (player-input-view
          #:on-name [on-name void]
          #:on-hp [on-hp void]
          #:name [name ""]
          #:hp [hp 1])
  (define/obs @name name)
  (define/obs @hp hp)
  (define (subtract-hp)
    (on-hp sub1)
    (<~@ @hp (switch
               [(<= 1) _]
               [else sub1])))
  (define (add-hp)
    (on-hp add1)
    (<@ @hp add1))
  (hpanel
    #:stretch '(#t #f)
    (input #:label "Name" @name (flow (~> 2> on-name))
           #:min-size '(200 #f))
    (counter (@~> @hp (~a "Max HP: " _)) add-hp subtract-hp)))

(module+ main
  (define (update-players players k f)
    (map (λ (e)
           (if (eq? (car e) k)
             (cons k (f (cdr e)))
             e))
         players))
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
  (define i-view
    (player-input-views
      (@> @players length)
      #:on-name (λ (k name)
                  (<~@ @players (update-players k (player-update-name name))))
      #:on-hp (λ (k f)
                (<~@ @players (update-players k (player-act-on-max-hp f))))
      #:names (map (flow (~> cdr player-name)) (@! @players))
      #:hps (map (flow (~> cdr player-max-hp)) (@! @players))))
  (void
    (with-closing-custodian/eventspace
      (render/eventspace
        #:eventspace closing-eventspace
        (window #:mixin close-custodian-mixin
                i-view)))
    ;; no separate eventspace: block main until this window closed
    (render/eventspace
      (window
        (list-view @players
          #:key car
          #:min-size (@~> @players (~>> length (* 100) (list #f)))
          (λ (k @e)
            (define (update proc)
              (<~@ @players (update-players k proc)))
            (player-view
              (@> @e cdr)
              #:on-condition (flow (~> player-condition-handler update))
              #:on-hp (flow (~> player-act-on-hp update))
              #:on-xp (flow (~> player-act-on-xp update))
              #:on-initiative (λ (i) (update (flow (player-set-initiative i))))
              #:on-summon
              (λ (name hp)
                (update (flow (player-summon name hp))))
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
