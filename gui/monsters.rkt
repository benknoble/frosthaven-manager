#lang racket

(provide
  (contract-out
    [single-monster-event/c contract?]
    [add-monster-event/c contract?]
    [remove-monster-event/c contract?]
    [single-monster-picker (->* (info-db/c (obs/c level/c))
                                (#:on-change (-> single-monster-event/c any)
                                 #:unavailable (set/c string?))
                                (is-a?/c view<%>))]
    [simple-monster-group-view (-> (obs/c monster-group?)
                                   (is-a?/c view<%>))]
    [multi-monster-picker (->* ((obs/c info-db/c) (obs/c level/c))
                               (#:on-change (-> (or/c add-monster-event/c
                                                      remove-monster-event/c)
                                                any))
                               (is-a?/c view<%>))]
    [monster-group-view
      (->* ((obs/c monster-group?)
            (obs/c (or/c #f monster-ability?))
            (obs/c (or/c #f monster-number/c)))
           (#:on-condition (-> monster-number/c condition? boolean?
                               any)
            #:on-hp (-> monster-number/c (-> number? number?)
                        any)
            #:on-kill (-> monster-number/c any)
            #:on-new (-> monster-number/c boolean? any)
            #:on-select (-> (or/c #f monster-number/c) any))
           (is-a?/c view<%>))]
    [db-view (-> (obs/c info-db/c) (obs/c ability-db/c) (is-a?/c view<%>)) ]))

(require racket/gui/easy
         racket/gui/easy/contract
         "../observable-operator.rkt"
         "mixins.rkt"
         "counter.rkt"
         "hierlist.rkt"

         "../qi.rkt"
         "../defns.rkt"
         "../monster-db.rkt")

(define single-monster-event/c
  (or/c
    (list/c 'set 'from string? 'to string?)
    (list/c 'monster 'from monster-info? 'to monster-info?)
    (list/c 'include? monster-number/c 'to boolean?)
    (list/c 'elite? monster-number/c 'to boolean?)
    (list/c 'level level/c)))

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
    (render ;; not setting current renderer
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

(define (monster-group-view @mg @ability @monster-num
                            #:on-select [on-select void]
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
    (render ;; not setting current renderer
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
          (button "Add" (λ () (close!)))))))
  (define name-initiative-panel
    (group
      "Initiative"
      (text (@> @mg monster-group-name))
      (text (@~> @ability (if monster-ability?
                            (~> monster-ability-initiative ~a)
                            "??")))
      (button "Add Monster" do-new)))
  (define ability-panel
    (group
      "Ability"
      #:min-size (list 200 #f)
      (vpanel
        (text
          (@~> @ability
               (if monster-ability?
                 (~>> (-< monster-ability-name
                          (~> (if monster-ability-shuffle?
                                " 🔀"
                                "")))
                      (format "~a~a"))
                 "")))
        (list-view (@~> @ability (if _
                                   monster-ability-abilities
                                   (gen empty)))
          (λ (k @e) (text (@~> @e (format "· ~a" _))))))))
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
    (on-kill (@! @monster-num)))
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
          [(select) (on-select (monster-number m))]))
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
      ability-panel
      stats-panel)
    monsters))

;; TODO: should be able to manipulate individual HP (? dialog with counter)
;; Takes a non-observable info-db b/c instantiated by a thunk in
;; multi-monster-picker, at which point we _want_ a fixed info-db.
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

(define (multi-monster-picker @info-db
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
  (define (add-monster-group)
    ;; 0: set
    ;; 1: info
    ;; 2: hash number -> elite
    ;; 3: level
    ;; Peeking @initial-level is valid because inside a button handler: the
    ;; value isn't accessed until after it is correctly set.
    ;; Peeking @info-db is valid because it is inside a button handler.
    (define-values (set info) (initial-set+info (@! @info-db)))
    (define new-group (vector set info (hash) (@! @initial-level)))
    (define (finish)
      (match-define (vector set info num->elite level) new-group)
      (when (and set info
                 (not (or (hash-empty? num->elite)
                          ;; valid because inside a dialog-closer: @monster-names won't
                          ;; update until the end of this form
                          (set-member? (@! @monster-names) (monster-info-name info)))))
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
    (define close! #f)
    (define (set-close! c) (set! close! c))
    (define-flow mixin
      (~> (make-closing-proc-mixin set-close!)
          (make-on-close-mixin finish)))
    (render ;; not setting current renderer
      (dialog
        #:mixin mixin
        #:title "Pick a Monster"
        ;; valid because inside a thunk: need to fix value
        #:min-size (~> (@info-db) @!
                       (-< longest-name-length longest-set-length)
                       + (* 10) (max 400) (list #f))
        (single-monster-picker
          ;; valid because inside a thunk: need to fix value
          (@! @info-db)
          @initial-level
          ;; valid because inside a dialog: @monster-names won't update until
          ;; the dialog is closed
          #:unavailable (@! @monster-names)
          #:on-change on-single-change)
        ;; On η-expansion of close!: close! can be #f until it is set, so
        ;; expand the call to close! (by the time it is called it should
        ;; have the correct value, a procedure).
        (button "Add" (λ () (close!))))))
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
    (~a (monster-current-hp monster))))

(define (db-view @info-db @ability-db)
  (define/obs @tab "Stats")
  (group
    "Monster DB"
    (text "If the Monster DB appears empty, then the provided file probably failed to load.")
    (tabs
      '("Stats" "Abilities")
      #:selection @tab
      (λ (e choices current)
        (case e
          [(select) (:= @tab current)]))
      (case-view @tab
        [("Stats") (info-view @info-db)]
        [("Abilities") (ability-view @ability-db)]
        [else (spacer)]))))

(define stats-table
  `([,monster-stats-max-hp "Max HP"]
    [,monster-stats-move "Move"]
    [,monster-stats-attack "Attack"]
    [,monster-stats-bonuses "Bonuses"]
    [,monster-stats-effects "Effects"]
    [,monster-stats-immunities "Immunities"]))
(define (fmt-stat s)
  (match s
    ['() "none"]
    [(list ss ...)
     (~> (ss)
         (map fmt-stat _)
         (string-join "; "))]
    [else (~a s)]))
(define (stats->hierlist* stats)
  (for/list ([func+label (in-list stats-table)])
    (match-define (list func label) func+label)
    (~a label ": " (fmt-stat (func stats)))))

(define (info->hierlist* info get-stats)
  (for/list ([(stats i) (in-indexed (in-list (get-stats info)))])
    `(item-list
       ,(~a "Level " i)
       ,(stats->hierlist* stats))))

(define (info-db->hierlist info-db)
  `(item-list
     "Stats"
     ,(for/list ([(set set-db) (in-hash info-db)])
        `(item-list
           ,set
           ,(for/list ([(group info) (in-hash set-db)])
              `(item-list
                 ,group
                 ((item-list
                    "Normal Stats"
                    ,(info->hierlist* info monster-info-normal-stats))
                  (item-list
                    "Elite Stats"
                    ,(info->hierlist* info monster-info-elite-stats)))))))))

(define (ability->hier-list ability)
  `(item-list
     ,(monster-ability-name ability)
     ,(list
        (~a "Initiative: " (monster-ability-initiative ability))
        (~a "Shuffle? " (if (monster-ability-shuffle? ability) "Yes" "No"))
        `(item-list
           "Abilities"
           ,(map ~a (monster-ability-abilities ability))))))

(define (ability-db->hierlist ability-db)
  `(item-list
     "Abilities"
     ,(for/list ([(set abilities) (in-hash ability-db)])
        `(item-list
           ,set
           ,(map ability->hier-list abilities)))))

(define (info-view @info-db)
  (hierlist
    (@> @info-db info-db->hierlist)))

(define (ability-view @ability-db)
  (hierlist
    (@> @ability-db ability-db->hierlist)))

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

(module+ main
  (define-values (info-db ability-db)
    (get-dbs default-monster-db))
  (void
    (render ;; not setting current renderer
      (window (db-view (@ info-db) (@ ability-db)))))
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
    (render ;; not setting current renderer
      (window
        (multi-monster-picker
          (@ info-db) (@ 3)
          #:on-change
          (match-lambda
            [`(add ,mg)
              (define/obs @mg mg)
              (define @ms (@> @mg monster-group-monsters))
              (define (get-first-monster)
                (and (not (empty? (@! @ms)))
                     (monster-number (first (@! @ms)))))
              (define/obs @n (get-first-monster))
              ;; actually, each same-set group across the whole scenario uses
              ;; the same deck (?), so it may be better to use state that looks
              ;; like ability-db + discarded-actions (same structure), or a
              ;; single state structured like actions-db but with deck + discard
              (define abilities-for-group
                (hash-ref ability-db (monster-group-set-name mg) empty))
              (define/obs @deck (shuffle abilities-for-group))
              (define/obs @discard empty)
              (define/obs @ability #f)
              (render ;; not setting current renderer
                (window
                  (monster-group-view
                    @mg @ability @n
                    #:on-condition
                    (λ (num c on?)
                      (<@ @mg (monster-group-update-num num (monster-update-condition c on?))))
                    #:on-hp
                    (λ (num proc)
                      (<@ @mg (monster-group-update-num num (monster-update-hp proc))))
                    #:on-kill
                    (λ (n)
                      (<@ @mg (monster-group-remove n))
                      (:= @n (get-first-monster)))
                    #:on-new
                    (λ (n elite?)
                      (<@ @mg (monster-group-add n elite?))
                      (:= @n n))
                    #:on-select (λ:= @n))
                  (hpanel
                    (button
                      "Draw"
                      (thunk
                        ;; draw a card
                        (:= @ability
                            ;; empty, so no cards at all, giving #f
                            ;; or there _must_ be cards to draw from
                            (~> (@deck) @! (and (not empty?) first)))
                        ;; update the deck
                        (<~@ @deck (switch [(not empty?) rest]))))
                    (button
                      "Next Round"
                      (thunk
                        ;; discard current card if it's a card
                        (when (monster-ability? (@! @ability))
                          (<~@ @discard (cons (@! @ability) _)))
                        ;; time to shuffle
                        (when (or (empty? (@! @deck))
                                  (and (monster-ability? (@! @ability))
                                       (monster-ability-shuffle? (@! @ability))))
                          (:= @deck
                              (shuffle
                                ;; @deck ++ @discard because @deck may not have
                                ;; been empty
                                (append (@! @deck) (@! @discard))))
                          (:= @discard empty))
                        ;; hide current ability
                        (:= @ability #f))))))]
            [_ (void)]))))))
