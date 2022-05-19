#lang racket

(provide
  (contract-out
    [info-db/c contract?]
    [action-db/c contract?]
    [get-dbs (-> path-string? (values info-db/c action-db/c))]
    [default-monster-db path-string?]))

(require racket/runtime-path
         "qi.rkt"
         "defns.rkt")

(define info-db/c
  (hash/c string? (hash/c string? monster-info?)))
(define action-db/c
  (hash/c string? (listof monster-action?)))

(define-runtime-path default-monster-db "monster-db.rktd")

(define (get-dbs db-file)
  (~> (db-file)
      file->list sep
      (partition
        ;; info db
        [monster-info? (~>> collect (group-by monster-info-set-name)
                            (list~>hash #:->key (~> car monster-info-set-name)
                                        #:->value (list~>hash #:->key monster-info-name)))]
        ;; actions deck
        [monster-action? (~>> collect (group-by monster-action-set-name)
                              (list~>hash #:->key (~> car monster-action-set-name)))])))

(define-flow take-first (~> hash-keys car))
(define (initial-set+info info-db)
  (define set (take-first info-db))
  (define name->info (hash-ref info-db set))
  (define info (hash-ref name->info (take-first name->info)))
  (values set info))

(define (longest-set-length info-db)
  (apply max (map string-length (hash-keys info-db))))

(define (longest-name-length info-db)
  (apply max
         (for*/list ([monster-name->monster-info (in-hash-values info-db)]
                     [monster-name (in-hash-keys monster-name->monster-info)])
           (string-length monster-name))))

(module+ gui
  (provide
    (contract-out
      [single-monster-event/c contract?]
      [add-monster-event/c contract?]
      [remove-monster-event/c contract?]
      [single-monster-picker (->* (info-db/c (obs/c (integer-in 0 max-level)))
                                  (#:on-change (-> single-monster-event/c any)
                                   #:unavailable (set/c string?))
                                  (is-a?/c view<%>))]
      [simple-monster-group-view (-> (obs/c monster-group?)
                                     (is-a?/c view<%>))]
      [multi-monster-picker (->* (info-db/c (obs/c (integer-in 0 max-level)))
                                 (#:on-change (-> (or/c add-monster-event/c
                                                        remove-monster-event/c)
                                                  any))
                                 (is-a?/c view<%>))]
      [monster-group-view
        (->* ((obs/c monster-group?)
              (obs/c (or/c #f monster-action?)))
             (#:on-condition (-> (integer-in 1 10) condition? boolean?
                                 any)
              #:on-hp (-> (integer-in 1 10) (-> number? number?)
                          any)
              #:on-kill (-> (integer-in 1 10) any)
              #:on-new (-> (integer-in 1 10) boolean? any))
             (is-a?/c view<%>))]))

  (require racket/gui/easy
           racket/gui/easy/contract
           "observable-operator.rkt"
           "gui/mixins.rkt"
           "gui/counter.rkt")

  (define single-monster-event/c
    (or/c
      (list/c 'set 'from string? 'to string?)
      (list/c 'monster 'from monster-info? 'to monster-info?)
      (list/c 'include? (integer-in 1 10) 'to boolean?)
      (list/c 'elite? (integer-in 1 10) 'to boolean?)
      (list/c 'level (integer-in 0 max-level))))

  (define (stats-view @stats)
    (vpanel
      (text (@~> @stats (~> monster-stats-move ~a)))
      (text (@~> @stats (~> monster-stats-attack ~a)))
      (text (@~> @stats (~> monster-stats-bonuses (string-join ", "))))
      (text (@~> @stats (~> monster-stats-effects (string-join ", "))))
      (text (@~> @stats (~> monster-stats-immunities (string-join ", "))))
      (text (@~> @stats (~> monster-stats-max-hp ~a)))))

  (define (monster-view @mg @monster
                        #:on-condition [on-condition void]
                        #:on-hp [on-hp void]
                        #:on-kill [on-kill void])
    (define @monster-stats (obs-combine get-monster-stats @mg @monster))
    (define (make-condition-checkbox c)
      (checkbox #:label (~a c)
                #:checked? (@~> @monster (~>> monster-conditions (member c) (not false?)))
                (flow (on-condition c _))))
    (define (show-conditions)
      (render
        (apply dialog
               #:title (obs-combine
                         (flow (~>> (== monster-group-name monster-number)
                                    (format "Conditions for ~a (~a)")))
                         @mg @monster)
               #:size '(400 #f)
               #:style '(close-button resize-border)
               (map make-condition-checkbox conditions))))
    (define (add-hp)
      (unless (@! (obs-combine monster-at-max-health? @monster @monster-stats))
        (on-hp add1)))
    (define (subtract-hp)
      (unless (@! (@> @monster monster-dead?))
        (on-hp sub1)))
    (hpanel
      (group
        "Stats"
        #:stretch '(#f #t)
        (text (@~> @monster (~>> monster-number (format "# ~a"))))
        (text (@~> @monster (if monster-elite? "Elite" "Normal")))
        (counter
          (obs-combine
            (flow (~>> (== monster-current-hp monster-stats-max-hp) (format "HP: ~a/~a")))
            @monster @monster-stats)
          add-hp
          subtract-hp)
        (button "💀Kill💀" on-kill))
      (group
        "Conditions"
        (text (@~> @monster (~> monster-conditions
                                (sep ~a) collect
                                (string-join ", " #:before-last " and "))))
        (button "Edit Conditions" show-conditions))))

  (define (monster-group-view @mg @action
                              #:on-condition [on-condition void]
                              #:on-hp [on-hp void]
                              #:on-kill [on-kill void]
                              #:on-new [on-new void])
    (define (do-new)
      (define @available-numbers
        (@~> @mg (~>> monster-group-monsters
                      (map monster-number)
                      (set-subtract (inclusive-range 1 10))
                      (sort _ <))))
      (define/obs @choice (~> (@available-numbers) @! (and (not empty?) first)))
      (define/obs @elite? #f)
      (define close! #f)
      (define (set-close! p) (set! close! p))
      (define (on-close)
        ;; valid because called when the dialog that changes @choice is closed
        (on-new (@! @choice) (@! @elite?)))
      (define-flow mixin (~> (make-closing-proc-mixin set-close!)
                             (make-on-close-mixin on-close)))
      (render
        (dialog
          #:mixin mixin
          #:title "Add Monster"
          #:style '(close-button resize-border)
          (hpanel
            (choice @available-numbers #:choice->label ~a (λ:= @choice))
            (checkbox #:label "Elite?" (λ:= @elite?))
            ;; On η-expansion of close!: close! can be #f until it is set, so
            ;; expand the call to close! (by the time it is called it should
            ;; have the correct value, a procedure).
            (button "Save" (λ () (close!)))))))
    (define name-initiative-panel
      (group
        "Initiative"
        (text (@> @mg monster-group-name))
        (text (@~> @action (if monster-action?
                             (~> monster-action-initiative ~a)
                             "??")))
        (button "Add Monster" do-new)))
    (define action-panel
      (group
        "Action"
        #:min-size (list 200 #f)
        (if-view @action
          (vpanel
            (text
              (@~> @action
                   (if monster-action?
                     (~>> (-< monster-action-name
                              (~> (if monster-action-shuffle?
                                    " 🔀"
                                    "")))
                          (format "~a~a"))
                     "")))
            (list-view (@~> @action (if _
                                      monster-action-abilities
                                      (gen empty)))
              (λ (k @e) (text (@~> @e (format "· ~a" _))))))
          (spacer))))
    (define stats-panel
      (hpanel
        (group "Normal" (stats-view (@> @mg monster-group-normal-stats))
               #:min-size (list (* 10 (string-length "Normal")) #f))
        (group "Stats"
               (text "Move")
               (text "Attack")
               (text "Bonuses")
               (text "Effects")
               (text "Immunities")
               (text "Max HP"))
        (group "Elite" (stats-view (@> @mg monster-group-elite-stats))
               #:min-size (list (* 10 (string-length "Elite")) #f))))
    ;; TODO: choice "hide"/"collapse" ?
    (define @monsters (@> @mg monster-group-monsters))
    (define (get-first-monster)
      (and (not (empty? (@! @monsters)))
           (monster-number (first (@! @monsters)))))
    (define/obs @monster-num (get-first-monster))
    (define @monster
      (obs-combine
        (λ (ms n) (and n (findf (flow (~> monster-number (= n))) ms)))
        @monsters @monster-num))
    (define-flow make-label-stats
      (-< (if monster-elite? " (E)" "")
          " (HP: " monster-current-hp ")"
          (if (~> monster-conditions empty?) "" "*")))
    (define (forward-condition c on?)
      (on-condition (@! @monster-num) c on?))
    (define (forward-hp proc)
      (on-hp (@! @monster-num) proc))
    (define (forward-kill)
      (on-kill (@! @monster-num))
      (unless (member (@! @monster-num) (map monster-number (@! @monsters)))
        (:= @monster-num (get-first-monster))))
    (define monsters
      (tabs
        @monsters
        #:selection @monster
        #:choice=? (flow (~> (>< monster-number) =))
        #:choice->label (flow (~> (-< monster-number make-label-stats) ~a))
        (λ (e ms m)
          (case e
            ;; no close: cannot close
            ;; no reorder: cannot reorder
            [(select) (:= @monster-num (monster-number m))]))
        (if-view @monster
          (monster-view
            @mg
            (@~> @monster (or _
                              ;; use a fill-in monster if none
                              (gen (monster 1 #f 0 empty))))
            #:on-condition forward-condition
            #:on-hp forward-hp
            #:on-kill forward-kill)
          (spacer))))
    (group
      "Monster"
      #:stretch '(#t #f)
      (hpanel
        #:alignment '(center center)
        #:margin '(20 0)
        name-initiative-panel
        action-panel
        stats-panel)
      monsters))

  ;; TODO: should be able to manipulate individual HP (? dialog with counter)
  (define (single-monster-picker info-db
                                 @initial-level
                                 #:on-change [on-change void]
                                 #:unavailable [unavailable empty])
    (define sets (hash-keys info-db))
    (define-values (set info) (initial-set+info info-db))
    (define/obs @set set)
    (define/obs @info info)
    (define @name->info (@~> @set (hash-ref info-db _)))
    (define (choose-set set)
      (on-change `(set from ,(@! @set) to ,set))
      (:= @set set))
    (define set-picker
      (choice #:label "Set" sets choose-set
              #:min-size (~> (info-db "Set")
                             (== longest-set-length string-length)
                             + (* 10) (max 50) (list #f))))
    (define @valid-monsters (@~> @name->info (~> hash-keys (set-subtract unavailable))))
    (define (choose-monster monster-name)
      (when monster-name
        (define new-info (hash-ref (@! @name->info) monster-name))
        (on-change `(monster from ,(@! @info) to ,new-info))
        (:= @info new-info)))
    ;; Set initial monster, which may not be info if info is already unavailable
    ;; according to @valid-monsters (or if the set operation re-orders the
    ;; keys…). Use #f like choice if no valid monster.
    (choose-monster (@! (@~> @valid-monsters (and (not empty?) first))))
    (define monster-picker
      (choice #:label "Monster" @valid-monsters choose-monster
              #:min-size (~> (info-db "Monster")
                             (== longest-name-length string-length)
                             + (* 10) (max 50) (list #f))))
    (define (make-monster-selector num)
      (define/obs @included? #f)
      (define (set-included included?)
        (on-change `(include? ,num to ,included?))
        (:= @included? included?))
      (define (set-elite elite?)
        (on-change `(elite? ,num to ,elite?)))
      (hpanel (checkbox set-included #:label (~a num))
              (checkbox set-elite #:label "Elite?" #:enabled? @included?)))
    (vpanel (hpanel set-picker monster-picker
                    #:alignment '(center top)
                    #:stretch '(#f #f))
            (slider @initial-level
                    (λ (level) (on-change `(level ,level)))
                    #:label "Level"
                    #:min-value 0
                    #:max-value max-level)
            (hpanel (apply vpanel (map make-monster-selector (inclusive-range 1 5)))
                    (apply vpanel (map make-monster-selector (inclusive-range 6 10))))))

  (define add-monster-event/c
    (list/c 'add monster-group?))
  (define remove-monster-event/c
    (list/c 'remove monster-group?))

  (define (multi-monster-picker info-db
                                @initial-level
                                #:on-change [on-change void])
    (define/obs @monster-groups empty)
    (define @monster-names
      (@~> @monster-groups
           (~> sep (>< (~> cdr monster-group-name)) collect)))
    (define/obs @next-id
      (@~> @monster-groups
           (~> sep (>< car) (rectify -1) max add1)))
    (define (make-simple-monster-group-view k @e)
      (define @m (@> @e cdr))
      (define @name (@> @m monster-group-name))
      (define (remove-group)
        (on-change `(remove ,(@! @m)))
        (<~@ @monster-groups (remove (@! @e) _)))
      (hpanel
        (vpanel #:stretch '(#f #t)
                (button (@~> @name (~a "Remove " _)) remove-group)
                (spacer))
        (simple-monster-group-view @m)))
    (define-values (set info) (initial-set+info info-db))
    (define (add-monster-group)
      ;; 0: set
      ;; 1: info
      ;; 2: hash number -> elite
      ;; 3: level
      ;; Peeking @initial-level is valid because inside a dialog-closer: the
      ;; value isn't accessed until after it is correctly set.
      (define new-group (vector set info (hash) (@! @initial-level)))
      (define (finish)
        (match-define (vector set info num->elite level) new-group)
        (when (not (or (hash-empty? num->elite)
                       ;; valid because inside a dialog-closer: @monster-names won't
                       ;; update until the end of this form
                       (set-member? (@! @monster-names) (monster-info-name info))))
          (define the-group
            (make-monster-group
              info level
              (hash->list num->elite)))
          (on-change `(add ,the-group))
          (<~@ @monster-groups (append (list (cons (@! @next-id) the-group))))))
      (define (on-single-change e)
        ;; update internal state
        (match e
          [`(set from ,old to ,new) (vector-set! new-group 0 new)]
          [`(monster from ,old to ,new) (vector-set! new-group 1 new)]
          [`(include? ,n to #t)
            (vector-update! new-group 2 (flow (hash-update n values #f)))]
          [`(include? ,n to #f)
            (vector-update! new-group 2 (flow (hash-remove n)))]
          [`(elite? ,n to ,elite?)
            ;; looks like hash-set, but I want the missing-key semantics of
            ;; hash-update with no failure-result as a guard against bugs
            (vector-update! new-group 2 (flow (hash-update n (const elite?))))]
          [`(level ,level) (vector-set! new-group 3 level)]))
      (render
        (dialog
          #:mixin (make-on-close-mixin finish)
          #:title "Pick a Monster"
          #:min-size (~> (info-db)
                         (-< longest-name-length longest-set-length)
                         + (* 10) (max 400) (list #f))
          (single-monster-picker
            info-db
            @initial-level
            ;; valid because inside a dialog: @monster-names won't update until
            ;; the dialog is closed
            #:unavailable (@! @monster-names)
            #:on-change on-single-change))))
    (vpanel
      (list-view @monster-groups
        #:key car
        ;; TODO edit monster
        make-simple-monster-group-view)
      (hpanel
        #:stretch '(#t #f)
        (spacer)
        (button "Add Monster" add-monster-group))))

  (define (simple-monster-group-view @monster-group)
    (vpanel
      #:alignment '(left top)
      (hpanel
        #:stretch '(#f #f)
        (text (@> @monster-group monster-group-name))
        (text (@~> @monster-group (~>> monster-group-level (~a "Level: ")))))
      (table
        '("Number" "Type" "HP")
        (@~> @monster-group (~> monster-group-monsters list->vector))
        void
        #:entry->row monster-info->row)))

  (define (monster-info->row monster)
    (vector
      (~a (monster-number monster))
      (if (monster-elite? monster)
        "Elite"
        "Normal")
      (~a (monster-current-hp monster)))))

(module+ main
  (require (submod ".." gui)
           racket/gui/easy
           "observable-operator.rkt")
  (define-values (info-db actions-db)
    (get-dbs "sample-db.rktd"))
  (define-values (set info) (initial-set+info info-db))
  (define/obs @state
    ;; 0: set
    ;; 1: info
    ;; 2: hash number -> elite
    (list set info (hash)))

  ;; (void
  ;;   (render
  ;;     (window
  ;;       (single-monster-picker
  ;;         info-db
  ;;         #:on-change
  ;;         (match-lambda
  ;;           [`(set from ,old to ,new) (<~@ @state (list-set 0 new))]
  ;;           [`(monster from ,old to ,new) (<~@ @state (list-set 1 new))]
  ;;           [`(include? ,n to #t)
  ;;             (<~@ @state (list-update 2 (flow (hash-update n values #f))))]
  ;;           [`(include? ,n to #f)
  ;;             (<~@ @state (list-update 2 (flow (hash-remove n))))]
  ;;           [`(elite? ,n to ,elite?)
  ;;             ;; looks like hash-set, but I want the missing-key semantics of
  ;;             ;; hash-update with no failure-result as a guard against bugs
  ;;             (<~@ @state (list-update 2 (flow (hash-update n (const elite?)))))])))))

  ;; (void
  ;;   (render
  ;;     (window
  ;;       (simple-monster-group-view
  ;;         (@> @state
  ;;             (match-lambda
  ;;               [(list set info num->elite?)
  ;;                (make-monster-group
  ;;                  info 3
  ;;                  (hash->list num->elite?))]))))))

  (void
    (render
      (window
        (multi-monster-picker
          info-db (@ 3)
          #:on-change
          (match-lambda
            [`(add ,mg)
              (define/obs @mg mg)
              ;; actually, each same-set group across the whole scenario uses
              ;; the same deck (?), so it may be better to use state that looks
              ;; like actions-db + discarded-actions (same structure), or a
              ;; single state structured like actions-db but with deck + discard
              (define actions-for-group
                (hash-ref actions-db (monster-group-set-name mg) empty))
              (define/obs @deck (shuffle actions-for-group))
              (define/obs @discard empty)
              (define/obs @action #f)
              (render
                (window
                  (monster-group-view
                    @mg @action
                    #:on-condition
                    (λ (num c on?)
                      (<@ @mg (monster-group-update-num num (monster-update-condition c on?))))
                    #:on-hp
                    (λ (num proc)
                      (<@ @mg (monster-group-update-num num (monster-update-hp proc))))
                    #:on-kill
                    (λ (n) (<@ @mg (monster-group-remove n)))
                    #:on-new
                    (λ (n elite?) (<@ @mg (monster-group-add n elite?))))
                  (hpanel
                    (button
                      "Draw"
                      (thunk
                        ;; draw a card
                        (:= @action
                            ;; empty, so no cards at all, giving #f
                            ;; or there _must_ be cards to draw from
                            (~> (@deck) @! (and (not empty?) first)))
                        ;; update the deck
                        (<~@ @deck (switch [(not empty?) rest]))))
                    (button
                      "Next Round"
                      (thunk
                        ;; discard current card if it's a card
                        (when (monster-action? (@! @action))
                          (<~@ @discard (cons (@! @action) _)))
                        ;; time to shuffle
                        (when (or (empty? (@! @deck))
                                  (and (monster-action? (@! @action))
                                       (monster-action-shuffle? (@! @action))))
                          (:= @deck
                              (shuffle
                                ;; @deck ++ @discard because @deck may not have
                                ;; been empty
                                (append (@! @deck) (@! @discard))))
                          (:= @discard empty))
                        ;; hide current action
                        (:= @action #f))))))]
            [_ (void)]))))))

(module+ test
  (require rackunit)
  (define-values (info actions)
    (get-dbs "sample-db.rktd"))
  (check-equal? info
                #hash(("archer"
                       .
                       #hash(("hynox archer"
                              .
                              #s(monster-info
                                  "archer"
                                  "hynox archer"
                                  (#s(monster-stats 2 2 2 () () ())
                                   #s(monster-stats 3 3 3 () () ())
                                   #s(monster-stats 4 4 4 () () ())
                                   #s(monster-stats 5 5 5 () () ())
                                   #s(monster-stats 6 6 6 () () ())
                                   #s(monster-stats 7 7 7 () () ())
                                   #s(monster-stats 8 8 8 () () ())
                                   #s(monster-stats 9 9 9 () () ()))
                                  (#s(monster-stats 4 2 3 ("shield 1") () ())
                                   #s(monster-stats 5 3 4 ("shield 1") () ())
                                   #s(monster-stats 6 4 5 ("shield 1") () ())
                                   #s(monster-stats 7 5 6 ("shield 2") () ())
                                   #s(monster-stats 8 6 7 ("shield 2") () ())
                                   #s(monster-stats 9 7 8 ("shield 2") () ())
                                   #s(monster-stats 10 8 9 ("shield 3") () ())
                                   #s(monster-stats 11 9 10 ("shield 3") () ()))))
                             ("wyrmling archer"
                              .
                              #s(monster-info
                                  "archer"
                                  "wyrmling archer"
                                  (#s(monster-stats 1 1 1 () () ())
                                   #s(monster-stats 2 2 2 () () ())
                                   #s(monster-stats 3 3 3 () () ())
                                   #s(monster-stats 4 4 4 () () ())
                                   #s(monster-stats 5 5 5 () () ())
                                   #s(monster-stats 6 6 6 () () ())
                                   #s(monster-stats 7 7 7 () () ())
                                   #s(monster-stats 8 8 8 () () ()))
                                  (#s(monster-stats 3 1 2 ("shield 1") () ())
                                   #s(monster-stats 4 2 3 ("shield 1") () ())
                                   #s(monster-stats 5 3 4 ("shield 1") () ())
                                   #s(monster-stats 6 4 5 ("shield 2") () ())
                                   #s(monster-stats 7 5 6 ("shield 2") () ())
                                   #s(monster-stats 8 6 7 ("shield 2") () ())
                                   #s(monster-stats 9 7 8 ("shield 3") () ())
                                   #s(monster-stats 10 8 9 ("shield 3") () ()))))))
                      ("guard"
                       .
                       #hash(("hynox guard"
                              .
                              #s(monster-info
                                  "guard"
                                  "hynox guard"
                                  (#s(monster-stats 2 2 2 () () ())
                                   #s(monster-stats 3 3 3 () () ())
                                   #s(monster-stats 4 4 4 () () ())
                                   #s(monster-stats 5 5 5 () () ())
                                   #s(monster-stats 6 6 6 () () ())
                                   #s(monster-stats 7 7 7 () () ())
                                   #s(monster-stats 8 8 8 () () ())
                                   #s(monster-stats 9 9 9 () () ()))
                                  (#s(monster-stats 4 2 3 ("shield 1") () ())
                                   #s(monster-stats 5 3 4 ("shield 1") () ())
                                   #s(monster-stats 6 4 5 ("shield 1") () ())
                                   #s(monster-stats 7 5 6 ("shield 2") () ())
                                   #s(monster-stats 8 6 7 ("shield 2") () ())
                                   #s(monster-stats 9 7 8 ("shield 2") () ())
                                   #s(monster-stats 10 8 9 ("shield 3") () ())
                                   #s(monster-stats 11 9 10 ("shield 3") () ()))))))))
  (check-equal? actions
                #hash(("archer"
                       .
                       (#s(monster-action
                            "archer"
                            "double-shot"
                            25
                            ("move +1"
                             "attack +2, range 5"
                             "attack +2, range 5, +1 if same target")
                            #f)
                        #s(monster-action
                            "archer"
                            "take aim"
                            80
                            ("move +2" "strengthen self")
                            #t)))
                      ("guard"
                       .
                       (#s(monster-action
                            "guard"
                            "rushing charge"
                            25
                            ("move +3" "attack +2 + number of spaces moved towards target")
                            #f)
                        #s(monster-action "guard" "stand tall" 80 ("shield 3") #t))))))
