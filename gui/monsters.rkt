#lang racket

(provide
  (contract-out
    [single-monster-event/c contract?]
    [add-monster-event/c contract?]
    [remove-monster-event/c contract?]
    [single-monster-picker (->* (info-db/c (obs/c level/c))
                                (#:on-change (-> single-monster-event/c any)
                                 #:unavailable (set/c string? #:cmp 'dont-care #:kind 'dont-care))
                                (is-a?/c view<%>))]
    [simple-monster-group-view (-> (obs/c monster-group?)
                                   (is-a?/c view<%>))]
    [multi-monster-picker (->* ((obs/c info-db/c) (obs/c level/c) (obs/c env/c))
                               (#:on-change (-> (or/c add-monster-event/c
                                                      remove-monster-event/c)
                                                any))
                               (is-a?/c view<%>))]
    [monster-group-view
      (->* ((obs/c monster-group?)
            (obs/c (or/c #f monster-ability?))
            (obs/c (or/c #f monster-number/c))
            (obs/c env/c))
           (#:on-condition (-> monster-number/c condition? boolean?
                               any)
            #:on-hp (-> monster-number/c (-> number? number?)
                        any)
            #:on-kill (-> monster-number/c any)
            #:on-new (-> monster-number/c boolean? any)
            #:on-select (-> (or/c #f monster-number/c) any))
           (is-a?/c view<%>))]
    [db-view (-> (obs/c info-db/c) (obs/c ability-db/c) (obs/c (listof monster-group?)) (is-a?/c view<%>))]
    [add-monster-group (->* ((obs/c info-db/c)
                             (obs/c level/c)
                             (obs/c (set/c string? #:cmp 'dont-care #:kind 'dont-care))
                             (obs/c env/c))
                            (#:on-group (-> monster-group? any))
                            any)]))

(require (only-in pict pict-width pict-height [text pict:text])
         racket/gui/easy
         racket/gui/easy/contract
         frosthaven-manager/observable-operator
         frosthaven-manager/gui/mixins
         frosthaven-manager/gui/counter
         frosthaven-manager/gui/stacked-tables
         frosthaven-manager/gui/render
         frosthaven-manager/gui/font

         frosthaven-manager/qi
         frosthaven-manager/defns
         frosthaven-manager/monster-db
         frosthaven-manager/parsers/formula)

(define single-monster-event/c
  (or/c
    (list/c 'set 'from string? 'to string?)
    (list/c 'monster 'from monster-info? 'to monster-info?)
    (list/c 'include? monster-number/c 'to boolean?)
    (list/c 'elite? monster-number/c 'to boolean?)
    (list/c 'level level/c)))

(define (stats-view @stats @env)
  (vpanel
    (text (@~> @stats (~> monster-stats-move ~a)))
    (text (obs-combine (flow (~> monster-stats-attack* ~a)) @stats @env))
    (cond-view
      [(@~> @stats (~> monster-stats-bonuses (not empty?)))
       (text (@~> @stats (~> monster-stats-bonuses (string-join ", "))))]
      [else (spacer)])
    (cond-view
      [(@~> @stats (~> monster-stats-effects (not empty?)))
       (text (@~> @stats (~> monster-stats-effects (string-join ", "))))]
      [else (spacer)])
    (cond-view
      [(@~> @stats (~> monster-stats-immunities (not empty?)))
       (text (@~> @stats (~> monster-stats-immunities (string-join ", "))))]
      [else (spacer)])
    (text (obs-combine (flow (~> monster-stats-max-hp* ~a)) @stats @env))))

(define (monster-view @mg @monster @env
                      #:on-condition [on-condition void]
                      #:on-hp [on-hp void]
                      #:on-kill [on-kill void])
  (define @monster-stats (obs-combine get-monster-stats @mg @monster))
  (define (make-condition-checkbox c)
    (checkbox #:label (~a c)
              #:checked? (@~> @monster (~>> monster-conditions (member c) (not false?)))
              (flow (on-condition c _))))
  (define (show-conditions)
    (with-closing-custodian/eventspace
      (render/eventspace
        #:eventspace closing-eventspace
        (apply window
               #:mixin close-custodian-mixin
               #:title (obs-combine
                         (flow (~>> (== monster-group-name monster-number)
                                    (format "Conditions for ~a (~a)")))
                         @mg @monster)
               #:size '(400 #f)
               (map make-condition-checkbox conditions)))))
  (define (add-hp)
    (unless (@! (obs-combine monster-at-max-health? @monster @monster-stats @env))
      (on-hp add1)))
  (define (subtract-hp)
    (unless (@! (@> @monster monster-dead?))
      (on-hp sub1)))
  (hpanel
    (vpanel
      #:stretch '(#f #t)
      (counter (obs-combine monster->hp-text @monster @monster-stats @env)
               add-hp
               subtract-hp)
      (button "💀Kill💀" on-kill))
    (vpanel
      (text (@~> @monster (~> monster-conditions
                              (sep ~a) collect
                              (string-join ", " #:before-last " and "))))
      (button "Edit Conditions" show-conditions))))

(define (monster-group-view @mg @ability @monster-num @env
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
    (define/obs @number->elite (hash))
    (define/match (on-change e)
      [{`(include? ,num to #t)} (<~@ @number->elite (hash-update num values #f))]
      [{`(include? ,num to #f)} (<~@ @number->elite (hash-remove num))]
      [{`(elite? ,num to ,elite?)}
       ;; looks like hash-set, but I want the missing-key semantics of
       ;; hash-update with no failure-result as a guard against bugs
       (<~@ @number->elite (hash-update num (const elite?)))])
    (define-close! close! closing-mixin)
    (define (on-close)
      ;; valid because called when the dialog that changes @number->elite is closed
      (for ([(num elite?) (in-hash (@! @number->elite))])
        (on-new num elite?)))
    (define-flow mixin (~> closing-mixin (make-on-close-mixin on-close)))
    ;; not setting current renderer, nor using an eventspace: dialog
    (render
     (dialog
      #:mixin mixin
      #:title "Add Monster"
      #:style '(close-button resize-border)
      (observable-view @available-numbers
                       (flow (~> sep (>< (make-monster-selector on-change))
                                 vpanel)))
      (button "Add" close!))))
  (define (name-panel) (text (@> @mg monster-group-name) #:font big-control-font))
  (define (add-monster-button)
    (button "Add Monster" do-new
            #:enabled?
            (@~> @mg (~>> monster-group-monsters
                          (map monster-number)
                          (set-subtract (inclusive-range 1 10))
                          (not empty?)))))
  (define (name-initiative-panel)
    (vpanel #:alignment '(center center)
            #:stretch '(#f #t)
            (name-panel)
            (text (@~> @ability monster-ability-initiative->text))
            (add-monster-button)))
  (define (ability-panel)
    (group
      "Ability"
      #:min-size (list 200 #f)
      (vpanel
        (text (@~> @ability monster-ability-name->text))
        (observable-view
         @ability
         (λ (ability)
           (apply vpanel
                  (for/list ([ability-text (if ability (monster-ability-abilities ability) empty)])
                    (hpanel (ability->text @mg ability-text @env)
                            (ability->extras @mg @ability ability-text)))))))))
  (define (stats-panel)
    (hpanel
      (group "Normal" (stats-view (@> @mg monster-group-normal-stats) @env)
             #:min-size (list (* 10 (string-length "Normal")) #f))
      (group "Stats"
             (text "Move")
             (text "Attack")
             (cond-view
               [(obs-combine (flow (~> (>< monster-stats-bonuses) (not (all empty?))))
                             (@> @mg monster-group-normal-stats)
                             (@> @mg monster-group-elite-stats))
                (text "Bonuses")]
               [else (spacer)])
             (cond-view
               [(obs-combine (flow (~> (>< monster-stats-effects) (not (all empty?))))
                             (@> @mg monster-group-normal-stats)
                             (@> @mg monster-group-elite-stats))
                (text "Effects")]
               [else (spacer)])
             (cond-view
               [(obs-combine (flow (~> (>< monster-stats-immunities) (not (all empty?))))
                             (@> @mg monster-group-normal-stats)
                             (@> @mg monster-group-elite-stats))
                (text "Immunities")]
               [else (spacer)])
             (text "Max HP"))
      (group "Elite" (stats-view (@> @mg monster-group-elite-stats) @env)
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
  (define-syntax-rule (define/forward/guard-monster-num f g args ...)
    (define (f args ...)
      (let ([n (@! @monster-num)])
        (when n
          (g n args ...)))))
  (define/forward/guard-monster-num forward-condition on-condition c on?)
  (define/forward/guard-monster-num forward-hp on-hp proc)
  (define/forward/guard-monster-num forward-kill on-kill)
  (define (monsters)
    (tabs
      @monsters
      #:selection @monster
      #:choice=? (flow (~> (>< monster-number) =))
      #:choice->label (flow (~> (-< monster-number make-label-stats) ~a))
      (λ (e _ms m)
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
          @env
          #:on-condition forward-condition
          #:on-hp forward-hp
          #:on-kill forward-kill)
        (spacer))))
  (group
    "Monster"
    #:stretch '(#t #f)
    (cond-view
      [(@> @monsters empty?) (hpanel (name-panel) (add-monster-button))]
      [else (vpanel (hpanel #:alignment '(center center)
                            (name-initiative-panel)
                            (stats-panel))
                    (ability-panel)
                    (monsters))])))

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
  (vpanel (hpanel set-picker monster-picker
                  #:alignment '(center top)
                  #:stretch '(#f #f))
          (slider @initial-level
                  (λ (level) (on-change `(level ,level)))
                  #:label "Level"
                  #:min-value 0
                  #:max-value max-level)
          (hpanel (apply vpanel (map (flow (make-monster-selector on-change)) (inclusive-range 1 5)))
                  (apply vpanel (map (flow (make-monster-selector on-change)) (inclusive-range 6 10))))))

;; on-change accepts
;; - `(include? ,num to ,included?)
;; - `(elite? ,num to ,elite?)
(define (make-monster-selector num [on-change void])
  (define/obs @included? #f)
  (define (set-included included?)
    (on-change `(include? ,num to ,included?))
    (:= @included? included?))
  (define (set-elite elite?)
    (on-change `(elite? ,num to ,elite?)))
  (hpanel #:alignment '(center top)
          (checkbox set-included #:label (~a num))
          (checkbox set-elite #:label "Elite?" #:enabled? @included?)))

(define add-monster-event/c
  (list/c 'add monster-group?))
(define remove-monster-event/c
  (list/c 'remove monster-group?))

(define (multi-monster-picker @info-db @initial-level @env #:on-change [on-change void])
  (define/obs @monster-groups empty)
  (define @monster-names
    (@~> @monster-groups
         (~> (sep (~> cdr monster-group-name)) collect)))
  (define/obs @next-id
    (@~> @monster-groups
         (~> (sep car) (rectify -1) max add1)))
  (define (make-simple-monster-group-view _k @e)
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
  (vpanel
    (list-view @monster-groups
      #:key car
      ;; TODO edit monster
      make-simple-monster-group-view)
    (hpanel
      #:stretch '(#t #f)
      (spacer)
      (button
        "Add Monster"
        (thunk
          (add-monster-group
            @info-db
            @initial-level
            @monster-names
            @env
            #:on-group (λ (g)
                         (on-change `(add ,g))
                         (<~@ @monster-groups (append (list (cons (@! @next-id) g)))))))))))


(define (add-monster-group @info-db @initial-level @monster-names @env #:on-group [on-group void])
  ;; 0: set
  ;; 1: info
  ;; 2: hash number -> elite
  ;; 3: level
  ;; Peeking @initial-level is valid because inside a button handler: the value
  ;; isn't accessed until after it is correctly set. Peeking @info-db is valid
  ;; because it is inside a button handler.
  (define-values (set info) (initial-set+info (@! @info-db)))
  (define new-group (vector set info (hash) (@! @initial-level)))
  (define (finish)
    (match-define (vector set info num->elite level) new-group)
    (when (and set info
               (not (or (hash-empty? num->elite)
                        ;; valid because inside a dialog-closer: @monster-names
                        ;; won't update until the end of this form
                        (set-member? (@! @monster-names) (monster-info-name info)))))
      (define the-group (make-monster-group info level (hash->list num->elite) (@! @env)))
      (on-group the-group)))
  (define (on-single-change e)
    ;; update internal state
    (match e
      [`(set from ,_old to ,new) (vector-set! new-group 0 new)]
      [`(monster from ,_old to ,new) (vector-set! new-group 1 new)]
      [`(include? ,n to #t) (vector-update! new-group 2 (flow (hash-update n values #f)))]
      [`(include? ,n to #f) (vector-update! new-group 2 (flow (hash-remove n)))]
      [`(elite? ,n to ,elite?)
        ;; looks like hash-set, but I want the missing-key semantics of
        ;; hash-update with no failure-result as a guard against bugs
        (vector-update! new-group 2 (flow (hash-update n (const elite?))))]
      [`(level ,level) (vector-set! new-group 3 level)]))
  (define-close! close! closing-mixin)
  (define-flow mixin (~> closing-mixin (make-on-close-mixin finish)))
  ;; not setting current renderer, nor using an eventspace: dialog
  (render
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
        ;; valid because inside a dialog: @monster-names won't update until the
        ;; dialog is closed
        #:unavailable (@! @monster-names)
        #:on-change on-single-change)
      (button "Add" close!))))

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

(define (db-view @info-db @ability-db @monster-groups)
  (define/obs @tab "Stats")
  (group
    "Monster DB"
    (tabs
      (@~> @monster-groups
           (switch
             [empty? '("Stats" "Abilities")]
             [_ '("Stats" "Abilities" "Foes")]))
      #:selection @tab
      (λ (e _choices current)
        (case e
          [(select) (:= @tab current)]))
      (case-view @tab
        [("Stats") (info-view @info-db)]
        [("Abilities") (ability-view @ability-db)]
        [("Foes") (foes-view @monster-groups)]
        [else (spacer)]))))

(define (info-view @info-db)
  (apply stacked-tables
         (@~> @info-db (~> hash-keys list->vector))
         info-view-stats-view
         (info-view-columns @info-db)))

;; (obs/c (or/c #f monster-stats?)) -> view
(define (info-view-stats-view @stats?)
  (apply vpanel
         (map (match-lambda
                [(list label func)
                 (hpanel (text (~a label ":"))
                         (text (@~> @stats? (if _ (~> func fmt-stat) "N/A"))))])
              stats-table)))

(define (info-view-columns @info-db)
  (struct data [set name level elite? info name->info])
  (list
    ;; set -> #((data set name _ _ _ name->info))
    (column "Set" values (λ (set)
                           (define name->info (hash-ref (@! @info-db) set))
                           (for/vector ([name (in-hash-keys name->info)])
                             (data set name #f #f #f name->info))))
    ;; (data set name _ _ _ name->info) -> #((date set name level elite? info name->info))
    (column "Name" data-name (match-lambda
                               [(data set name _ _ _ name->info)
                                (define mi (hash-ref name->info name))
                                (for*/vector ([level (in-range number-of-levels)]
                                              [elite? (list #t #f)])
                                  (data set name level elite? mi name->info))]))
    ;; (date set name level elite? info name->info) -> monster-stats?
    (column "Level"
            (match-lambda [(data _ _ level elite? _ _)
                           (~a "Level " level (if elite? " (Elite)" ""))])
            (match-lambda
              [(data _ _ level elite? info _)
               (define ->stats (if elite? monster-info-elite-stats monster-info-normal-stats))
               (~> (info) ->stats (list-ref level))]))))

(define stats-table
  `(["Max HP" ,monster-stats-max-hp]
    ["Move" ,monster-stats-move]
    ["Attack" ,monster-stats-attack]
    ["Bonuses" ,monster-stats-bonuses]
    ["Effects" ,monster-stats-effects]
    ["Immunities" ,monster-stats-immunities]))

(define (fmt-stat s)
  (match s
    ['() "none"]
    [(list ss ...)
     (~> (ss)
         (map fmt-stat _)
         (string-join "; "))]
    [_ (~a s)]))

(define (ability-view @ability-db)
  (apply stacked-tables
         #:panel vpanel
         (@~> @ability-db (~> hash-keys list->vector))
         ability-view-ability-view
         (ability-view-columns @ability-db)))

;; (obs/c (or/c #f monster-ability?)) -> view
(define (ability-view-ability-view @ability?)
  (apply vpanel
         (map (match-lambda
                [(list label func)
                 (hpanel (text label) (text (@~> @ability? (if _ func "N/A"))))])
              ability-table)))

(define (ability-view-columns @ability-db)
  (list
    ;; set -> #(monster-ability?)
    (column "Set" values (λ (set)
                           (~> (@ability-db) @! (hash-ref set) list->vector)))
    ;; monster-ability? -> monster-ability?
    (column "Ability" monster-ability-name values)))

(define ability-table
  `(["Initiative:" ,(flow (~> monster-ability-initiative ~a))]
    ["Shuffle?" ,(flow (if monster-ability-shuffle? "Yes" "No"))]
    ["Abilities:" ,(flow (~> monster-ability-abilities (string-join "\n")))]))

(define (foes-view @monster-groups)
  (list-view @monster-groups
    (λ (_k @e) (simple-monster-group-view @e))))

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

(define aoe-rx #rx"aoe\\(([^)]+)\\)")

(define (ability->text @mg ability @env)
  (text (obs-combine (monster-ability-ability->text ability) @mg @env)))

(define (aoe-button pict)
  (button "AoE" (thunk
                 (with-closing-custodian/eventspace
                  (render/eventspace
                   #:eventspace closing-eventspace
                   (window
                    #:mixin close-custodian-mixin
                    #:title "AoE pattern"
                    #:size (~> (pict)
                               (-< pict-width pict-height)
                               (>< exact-ceiling) list)
                    (pict-canvas pict values)))))))

(define (ability->extras @mg @ability-card ability-text)
  (define @extras
    (@~> @ability-card (monster-ability-ability->extras ability-text)))
  (observable-view
   @extras
   (λ (extras)
     (apply hpanel
            (for/list ([extra extras])
              (match extra
                [(list 'aoe-pict pict) (aoe-button pict)]))))))

(module+ main
  (require frosthaven-manager/gui/render)
  (define-values (info-db ability-db)
    (get-dbs default-monster-db))
  (void
    (with-closing-custodian/eventspace
      (render/eventspace
        #:eventspace closing-eventspace
        (window #:mixin close-custodian-mixin
                #:title "Info DB"
                (db-view (@ info-db) (@ ability-db) (@ (list)))))))
  (define-values (set info) (initial-set+info info-db))
  (define/obs @state
    ;; 0: set
    ;; 1: info
    ;; 2: hash number -> elite
    (list set info (hash)))

  (void
    (with-closing-custodian/eventspace
      (render/eventspace
        #:eventspace closing-eventspace
        (window
          #:title "Simple picker"
          #:mixin close-custodian-mixin
          (single-monster-picker
            info-db (@ 3)
            #:on-change
            (match-lambda
              [`(set from ,_old to ,new) (<~@ @state (list-set 0 new))]
              [`(monster from ,_old to ,new) (<~@ @state (list-set 1 new))]
              [`(include? ,n to #t)
                (<~@ @state (list-update 2 (flow (hash-update n values #f))))]
              [`(include? ,n to #f)
                (<~@ @state (list-update 2 (flow (hash-remove n))))]
              [`(elite? ,n to ,elite?)
                ;; looks like hash-set, but I want the missing-key semantics of
                ;; hash-update with no failure-result as a guard against bugs
                (<~@ @state (list-update 2 (flow (hash-update n (const elite?)))))])))))

    (with-closing-custodian/eventspace
      (render/eventspace
        #:eventspace closing-eventspace
        (window
          #:title "Simple picker view"
          #:mixin close-custodian-mixin
          (simple-monster-group-view
            (@> @state
                (match-lambda
                  [(list _set info num->elite?)
                   (make-monster-group info 3 (hash->list num->elite?) #hash())])))))))

  (void
    ;; no separate eventspace: block main until this window closed
    (render/eventspace
      (window
        #:title "Multi Picker"
        (multi-monster-picker
          (@ info-db) (@ 3) (@ #hash())
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
              (define/obs @draw? #t)
              (with-closing-custodian/eventspace
                (render/eventspace
                  #:eventspace closing-eventspace
                  (window
                    #:title "Monster view"
                    #:mixin close-custodian-mixin
                    (monster-group-view
                      @mg @ability @n (@ #hash())
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
                        (<@ @mg (monster-group-add n elite? #hash()))
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
                          (<~@ @deck (switch [(not empty?) rest]))
                          (<@ @draw? not))
                        #:enabled? @draw?)
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
                          (:= @ability #f)
                          (<@ @draw? not))
                        #:enabled? (@> @draw? not))))))]
            [_ (void)]))))))
