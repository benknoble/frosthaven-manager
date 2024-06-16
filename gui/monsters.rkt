#lang racket

(provide
  (contract-out
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
            (obs/c ability-decks?)
            (obs/c (or/c #f monster-number/c))
            (obs/c env/c))
           (#:on-condition (-> monster-number/c condition? boolean?
                               any)
            #:on-hp (-> monster-number/c (-> number? number?)
                        any)
            #:on-kill (-> monster-number/c any)
            #:on-new (-> monster-number/c boolean? any)
            #:on-select (-> (or/c #f monster-number/c) any)
            #:on-swap (-> (or/c 'all monster-number/c) any)
            #:on-move-ability-card (-> any)
            #:on-max-hp (-> (-> (or/c 'normal 'elite) natural-number/c number?) any)
            #:on-change-level (-> level/c any)
            #:on-update (-> (-> monster-group? monster-group?) any))
           (is-a?/c view<%>))]
    [db-view (-> (obs/c info-db/c) (obs/c ability-db/c) (obs/c (listof monster-group?)) (is-a?/c view<%>))]
    [add-monster-group (->* ((obs/c info-db/c)
                             (obs/c level/c)
                             (obs/c (set/c string? #:cmp 'dont-care #:kind 'dont-care))
                             (obs/c env/c))
                            (#:on-group (-> monster-group? any))
                            any)]))

(require (only-in pict pict-width pict-height)
         racket/gui/easy
         racket/gui/easy/contract
         frosthaven-manager/observable-operator
         frosthaven-manager/gui/mixins
         frosthaven-manager/gui/counter
         frosthaven-manager/gui/stacked-tables
         frosthaven-manager/gui/render
         frosthaven-manager/gui/font
         frosthaven-manager/gui/helpers
         frosthaven-manager/gui/table
         frosthaven-manager/gui/rich-text-display
         frosthaven-manager/gui/pict-text-display
         frosthaven-manager/gui/level-picker

         frosthaven-manager/defns
         frosthaven-manager/manager
         frosthaven-manager/monster-db
         frosthaven-manager/parsers/formula)

(module+ test (require rackunit))

(define (stats-view @stats @env)
  (define (empty-view f sf)
    (cond-view
      [(@> @stats {~> f (not empty?)}) (text (@> @stats {~> sf escape-text}))]
      [else (spacer)]))
  (vpanel
    (text (@> @stats {~> monster-stats-move (if _ ~a "-")}))
    (text (obs-combine {~> monster-stats-attack* ~a} @stats @env))
    (empty-view monster-stats-bonuses monster-stats-bonuses-string)
    (empty-view monster-stats-effects monster-stats-effects-string)
    (empty-view monster-stats-immunities monster-stats-immunities-string)
    (text (obs-combine {~> monster-stats-max-hp* ~a} @stats @env))))

(define (monster-view @mg @monster @env
                      #:on-condition [on-condition void]
                      #:on-hp [on-hp void]
                      #:on-kill [on-kill void]
                      #:on-swap [on-swap void])
  (define @monster-stats (obs-combine get-monster-stats @mg @monster))
  (define (make-condition-checkbox c)
    (checkbox #:label (format-condition c)
              #:checked? (@> @monster {~>> monster-conditions (member c) (not false?)})
              {(on-condition c _)}))
  (define (show-conditions)
    (with-closing-custodian/eventspace
      (render/eventspace
        #:eventspace closing-eventspace
        (apply window
               #:mixin close-custodian-mixin
               #:title (obs-combine
                        {~>> (== monster-group-name monster-number)
                             (format "Conditions for ~a (~a)")}
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
    (button (@> @monster {~>> (if monster-elite? "Normal" "Elite")
                              (~a "Swap to ")})
            on-swap)
    (vpanel
      (rich-text-display (@> @monster {~> monster-conditions conditions->string list})
                         #:min-size '(50 30))
      (button "Edit Conditions" show-conditions))))

(define (monster-group-view @mg @ability-deck @monster-num @env
                            #:on-select [on-select void]
                            #:on-condition [on-condition void]
                            #:on-hp [on-hp void]
                            #:on-kill [on-kill void]
                            #:on-new [on-new void]
                            #:on-swap [on-swap void]
                            #:on-move-ability-card [on-move-ability-card void]
                            #:on-max-hp [on-max-hp void]
                            #:on-change-level [on-change-level void]
                            #:on-update [arbitrary-update void])
  (define @ability (@> @ability-deck ability-decks-current))
  (define @monsters (@> @mg monster-group-monsters))
  (define @monster
    (obs-combine
      (λ (ms n) (and n (findf {~> monster-number (= n)} ms)))
      @monsters @monster-num))
  (define/forward/guard-monster-num @monster-num forward-condition on-condition c on?)
  (define/forward/guard-monster-num @monster-num forward-hp on-hp proc)
  (define/forward/guard-monster-num @monster-num forward-kill on-kill)
  (define/forward/guard-monster-num @monster-num forward-swap on-swap)
  (group
    "Monster"
    #:stretch '(#t #f)
    (cond-view
      [(@> @monsters empty?) (hpanel (name-panel @mg) (add-monster-button @mg on-new))]
      [else (vpanel (hpanel #:alignment '(center center)
                            (name-initiative-panel @mg @env @ability @ability-deck
                                                   #:on-new on-new
                                                   #:arbitrary-update arbitrary-update
                                                   #:on-swap on-swap
                                                   #:on-move-ability-card on-move-ability-card
                                                   #:on-max-hp on-max-hp
                                                   #:on-change-level on-change-level)
                            (stats-panel @mg @env))
                    (ability-panel @mg @env @ability)
                    (monsters-panel @mg @env @monsters @monster
                                    #:on-select on-select
                                    #:on-condition forward-condition
                                    #:on-hp forward-hp
                                    #:on-kill forward-kill
                                    #:on-swap forward-swap))])))

(define (monster-ability-view @ability @mg @env)
  (pict-text-display
   (obs-combine
    (λ (ability mg env)
      (if ability
        (list*
         (monster-ability-name->text ability) " (" (monster-ability-initiative->text ability) ")" newline
         (~> (ability)
             (if _ monster-ability-abilities '())
             (sep (-< (~> (monster-ability-ability->rich-text ability mg env) sep)
                      (gen newline)))
             collect
             (dropf-right newline?)))
        (list "???")))
    @ability @mg @env)
   #:min-size '(200 60)))


(define ((do-new @mg on-new))
  (define @available-numbers
    (@> @mg {~>> monster-group-monsters
                 (map monster-number)
                 (set-subtract (inclusive-range 1 10))
                 (sort _ <)}))
  (define/obs @number->elite (hash))
  (define (on-change e)
    (<@ @number->elite {(update-selected-tracker e _)}))
  (define-close! close! closing-mixin)
  (define (on-close)
    ;; valid because called when the dialog that changes @number->elite is closed
    (for ([(num elite?) (in-hash (@! @number->elite))])
      (on-new num elite?)))
  (define mixin {~> closing-mixin (esc (make-on-close-mixin on-close))})
  ;; not setting current renderer, nor using an eventspace: dialog
  (render
   (dialog
    #:mixin mixin
    #:title "Add Monster"
    #:style '(close-button resize-border)
    (observable-view @available-numbers
                     {~> sep (>< (make-monster-selector on-change))
                         vpanel})
    (button "Add" close!))))

(define ((do-mass-condition @mg arbitrary-update))
  (define-close! close! closing-mixin)
  (define ((do-it on?))
    ;; valid because inside a dialog closer
    (define c (@! @condition))
    (arbitrary-update
     (λ (mg)
       (for/fold ([mg mg])
                 ([monster (monster-group-monsters mg)])
         ((monster-group-update-num (monster-number monster)
                                    (monster-update-condition c on?))
          mg))))
    (close!))
  (define add (do-it #t))
  (define remove (do-it #f))
  (define/obs @condition (first conditions))
  ;; not setting current renderer, nor using an eventspace: dialog
  (render
   (dialog
    #:min-size '(400 #f)
    #:mixin closing-mixin
    #:title (@> @mg {~>> monster-group-name escape-text (~a "Mass Assign Conditions for ")})
    (choice conditions (λ:= @condition) #:choice->label format-condition #:selection @condition)
    (hpanel
     (button "Add" add)
     (button "Remove" remove)
     (button "Cancel" close!)))))

(define ((do-expire-conditions arbitrary-update))
  (arbitrary-update
   (λ (mg)
     (for/fold ([mg mg])
               ([monster (monster-group-monsters mg)])
       ((monster-group-update-num (monster-number monster) monster-expire-conditions)
        mg)))))

(define ((change-max-hp @mg @env on-max-hp arbitrary-update))
  (define-close! close! closing-mixin)
  (define/obs @inc #f)
  (define (change!)
    (define inc (@! @inc))
    (when inc
      (define env (@! @env))
      (define true-inc
        (with-handlers ([exn:fail? (const 0)])
          ((parse-expr inc) env)))
      (on-max-hp (λ (_type num) (+ num true-inc)))
      (arbitrary-update
       (λ (mg)
         (for/fold ([mg mg])
                   ([monster (monster-group-monsters mg)])
           ((monster-group-update-num (monster-number monster)
                                      (monster-update-hp {(+ true-inc)}))
            mg)))))
    (close!))
  ;; not setting current renderer, nor using an eventspace: dialog
  (render
   (dialog
    #:min-size '(400 #f)
    #:mixin closing-mixin
    #:title (@> @mg {~>> monster-group-name escape-text (~a "Change maximum HP for ")})
    #:style '()
    (input ""
           (match-lambda**
             [{'input val} (:= @inc val)]
             [{'return val} (:= @inc val)
                            (change!)])
           #:label "Enter a number or a formula to add")
    (hpanel
     (button "Change" change!)
     (button "Cancel" close!)))))

(define ((change-level @mg on-change-level))
  (define-close! close! closing-mixin)
  (define/obs @new-level #f)
  (define (change!)
    (close!)
    (define new-level (@! @new-level))
    (when new-level
      (on-change-level new-level)))
  ;; not setting current renderer, nor using an eventspace: dialog
  (render
   (dialog
    #:mixin closing-mixin
    #:title (@> @mg {~>> monster-group-name (~a "Change level for")})
    #:min-size '(300 #f)
    (level-picker #:choose (λ:= @new-level) #:selection (@> @mg monster-group-level))
    (hpanel
     (button "Ok" change!)
     (button "Cancel" close!)))))

(define (name-panel @mg)
  (text (@> @mg {~> monster-group-name escape-text}) #:font big-control-font))


(define (add-monster-button @mg on-new)
  (button "Add Monster" (do-new @mg on-new)
          #:enabled?
          (@> @mg {~>> monster-group-monsters
                       (map monster-number)
                       (set-subtract (inclusive-range 1 10))
                       (not empty?)})))

(define (name-initiative-panel @mg
                               @env
                               @ability
                               @ability-deck
                               #:on-new on-new
                               #:arbitrary-update arbitrary-update
                               #:on-swap on-swap
                               #:on-move-ability-card on-move-ability-card
                               #:on-max-hp on-max-hp
                               #:on-change-level on-change-level)
  (vpanel #:alignment '(center center)
          #:stretch '(#f #t)
          (name-panel @mg)
          (text (@> @ability monster-ability-initiative->text))
          (add-monster-button @mg on-new)
          (button "More Actions…"
                  (thunk
                   ;; not setting current renderer, nor using an eventspace: dialog
                   (render
                    (dialog
                     #:min-size '(400 #f)
                     #:title (@> @mg {~>> monster-group-name escape-text (~a "More Actions for ")})
                     (button "Expire Conditions" (do-expire-conditions arbitrary-update))
                     (button "Swap Elite/Normal" (thunk (on-swap 'all)))
                     (button "Mass Conditions" (do-mass-condition @mg arbitrary-update))
                     (ability-deck-preview @ability-deck @mg @env #:on-move on-move-ability-card)
                     (button "Change all maximum HP"
                             (change-max-hp @mg @env on-max-hp arbitrary-update))
                     (button "Change level" (change-level @mg on-change-level))))))))

(define (stats-panel @mg @env)
  (define (empty-stats label f)
    (cond-view
      [(obs-combine {~> (>< f) (not (all empty?))}
                    (@> @mg monster-group-normal-stats)
                    (@> @mg monster-group-elite-stats))
       (text label)]
      [else (spacer)]))
  (hpanel
   (group "Normal" (stats-view (@> @mg monster-group-normal-stats) @env)
          #:min-size (list (* 10 (string-length "Normal")) #f))
   (group "Stats"
          (text "Move")
          (text "Attack")
          (empty-stats "Bonuses" monster-stats-bonuses)
          (empty-stats "Effects" monster-stats-effects)
          (empty-stats "Immunities" monster-stats-immunities)
          (text "Max HP"))
   (group "Elite" (stats-view (@> @mg monster-group-elite-stats) @env)
          #:min-size (list (* 10 (string-length "Elite")) #f))))

(define (monsters-panel @mg @env @monsters @monster
                        #:on-select on-select
                        #:on-condition forward-condition
                        #:on-hp forward-hp
                        #:on-kill forward-kill
                        #:on-swap forward-swap)
  (tabs
   @monsters
   #:selection @monster
   #:choice=? {~> (>< monster-number) =}
   #:choice->label {~> (-< monster-number make-label-stats) ~a}
   (λ (e _ms m)
     (case e
       ;; no close: cannot close
       ;; no reorder: cannot reorder
       [(select) (on-select (monster-number m))]))
   (if-view @monster
     (monster-view
      @mg
      (@> @monster {(or _
                        ;; use a fill-in monster if none
                        (gen (monster 1 #f 0 empty)))})
      @env
      #:on-condition forward-condition
      #:on-hp forward-hp
      #:on-kill forward-kill
      #:on-swap forward-swap)
     (spacer))))

(define-flow make-label-stats
  (-< (if monster-elite? " (E)" "")
      " (HP: " monster-current-hp ")"
      (if (~> monster-conditions empty?) "" "*")))

(define-syntax-rule (define/forward/guard-monster-num @monster-num f g args ...)
  (define (f args ...)
    (let ([n (@! @monster-num)])
      (when n
        (g n args ...)))))

(define (ability-panel @mg @env @ability)
  (group
   "Ability"
   #:min-size (list 200 #f)
   (monster-ability-view @ability @mg @env)))

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
  (define @name->info (@> @set {(hash-ref info-db _)}))
  (define (choose-set set)
    (on-change `(set from ,(@! @set) to ,set))
    (:= @set set))
  (define set-picker
    (choice #:label "Set" (sort sets string<?) choose-set
            #:min-size (~> (info-db "Set")
                           (== longest-set-length string-length)
                           + (* 10) (max 50) (list #f))))
  (define @valid-monsters (@> @name->info {~> hash-keys (set-subtract unavailable) (sort string<?)}))
  (define (choose-monster monster-name)
    (when monster-name
      (define new-info (hash-ref (@! @name->info) monster-name))
      (on-change `(monster from ,(@! @info) to ,new-info))
      (:= @info new-info)))
  ;; Set initial monster, which may not be info if info is already unavailable
  ;; according to @valid-monsters (or if the set operation re-orders the
  ;; keys…). Use #f like choice if no valid monster.
  (choose-monster (@! (@> @valid-monsters {(and (not empty?) first)})))
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
          (hpanel (apply vpanel (map {(make-monster-selector on-change)} (inclusive-range 1 5)))
                  (apply vpanel (map {(make-monster-selector on-change)} (inclusive-range 6 10))))))

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

(define (multi-monster-picker @info-db @initial-level @env #:on-change [on-change void])
  ;; TODO why maintain own list? why not have passed in? (something to do with IDs?)
  (define/obs @monster-groups empty)
  (define @monster-names
    (@> @monster-groups
        {~> (sep (~> cdr monster-group-name)) collect}))
  (define/obs @next-id
    (@> @monster-groups
         {~> (sep car) (rectify -1) max add1}))
  (define (make-simple-monster-group-view _k @e)
    (define @m (@> @e cdr))
    (define @name (@> @m monster-group-name))
    (define (remove-group)
      (on-change `(remove ,(@! @m)))
      (<@ @monster-groups {(remove (@! @e) _)}))
    (hpanel
      (vpanel #:stretch '(#f #t)
              (button (@> @name {(~a "Remove " _)}) remove-group)
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
                         (<@ @monster-groups {(append (list (cons (@! @next-id) g)))}))))))))


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
      [(or `(include? ,_ to ,_) `(elite? ,_ to ,_)) (vector-update! new-group 2 {(update-selected-tracker e _)})]
      [`(level ,level) (vector-set! new-group 3 level)]))
  (define-close! close! closing-mixin)
  (define mixin {~> closing-mixin (esc (make-on-close-mixin finish))})
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
      (text (@> @monster-group {~> monster-group-name escape-text}))
      (text (@> @monster-group {~>> monster-group-level (~a "Level: ")})))
    (table
      '("Number" "Type" "HP")
      (@> @monster-group {~> monster-group-monsters list->vector})
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
     (@> @monster-groups
         {switch
           [empty? '("Stats" "Abilities")]
           [_ '("Stats" "Abilities" "Foes")]})
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
         (@> @info-db {~> hash-keys (sort string<?) list->vector})
         info-view-stats-view
         (info-view-columns @info-db)))

;; (obs/c (or/c #f monster-stats?)) -> view
(define (info-view-stats-view @stats?)
  (apply vpanel
         (map (match-lambda
                [(list label func)
                 (hpanel (text (~a label ":"))
                         (text (@> @stats? {(if _ (~> func fmt-stat escape-text) "N/A")})))])
              stats-table)))

(define (info-view-columns @info-db)
  (struct data [set name level elite? info name->info])
  (list
    ;; set -> #((data set name _ _ _ name->info))
    (column "Set" values (λ (set)
                           (define name->info (hash-ref (@! @info-db) set))
                           (for/vector ([name (sort (hash-keys name->info) string<?)])
                             (data set name #f #f #f name->info))))
    ;; (data set name _ _ _ name->info) -> #((data set name level elite? info name->info))
    (column "Name" data-name (match-lambda
                               [(data set name _ _ _ name->info)
                                (define mi (hash-ref name->info name))
                                (for*/vector ([level (in-range number-of-levels)]
                                              [elite? (list #t #f)])
                                  (data set name level elite? mi name->info))]))
    ;; (data set name level elite? info name->info) -> monster-stats?
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
    [#f "-"]
    ['() "none"]
    [(list ss ...)
     (~> (ss)
         (map fmt-stat _)
         (string-join "; "))]
    [_ (~a s)]))

(define (ability-view @ability-db)
  (apply stacked-tables
         #:panel vpanel
         (@> @ability-db {~> hash-keys (sort string<?) list->vector})
         ability-view-ability-view
         (ability-view-columns @ability-db)))

;; (obs/c (or/c #f monster-ability?)) -> view
(define (ability-view-ability-view @ability?)
  (define from-table
    (map (match-lambda
           [(list label func)
            (hpanel (text label) (text (@> @ability? {(if _ (~> func escape-text) "N/A")})))])
         ability-table))
  (define others
    (list
     (hpanel
      (text "Abilities:")
      (observable-view
       @ability?
       (λ (ability?)
         (apply vpanel
                (for/list ([ability-text (if ability? (monster-ability-abilities ability?) empty)])
                  (text (escape-text ability-text)))
                #:alignment '(left center)))))))
  (apply vpanel (append from-table others)))

(define (ability-view-columns @ability-db)
  (list
    ;; set -> #(monster-ability?)
    (column "Set" values (λ (set)
                           (~> (@ability-db) @! (hash-ref set) list->vector)))
    ;; monster-ability? -> monster-ability?
    (column "Ability" monster-ability-name values)))

(define ability-table
  `(["Initiative:" ,{~> monster-ability-initiative ~a}]
    ["Shuffle?" ,{(if monster-ability-shuffle? "Yes" "No")}]))

(define (foes-view @monster-groups)
  (list-view @monster-groups
    (λ (_k @e) (simple-monster-group-view @e))))

(define take-first {~> hash-keys (sort string<?) car})
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

(define (ability-deck-preview @ability-deck @mg @env #:on-move [on-move void])
  (define (make-discard-rows ability-deck)
    (for/vector ([ability (ability-decks-discard ability-deck)])
      (vector (monster-ability-name->text ability))))
  (define (card-information @cards @selection @revealed @mg @env)
    (cond-view
      [(obs-combine {(and 1> (<= 0 __))}
                    @selection
                    @revealed)
       (monster-ability-view (obs-combine {(if (and 2> (~> X (== _ length) <))
                                               list-ref
                                               (gen (monster-ability "" "" 0 empty #f #f)))}
                                          @cards
                                          @selection)
                             @mg
                             @env)]
      [else (spacer)]))
  (define (revealed-ability-card-information @ability-deck @draw-selection @revealed @mg @env)
    (define @cards (@> @ability-deck ability-decks-draw))
    (card-information @cards @draw-selection (@> @revealed {switch [number? sub1] [else +inf.0]}) @mg @env))
  (define (discard-ability-card-information @ability-deck @discard-selection @mg @env)
    (define @cards (@> @ability-deck ability-decks-discard))
    (card-information @cards @discard-selection (@> @cards length) @mg @env))
  (button
   "Preview Ability Deck"
   (thunk
    (define/obs @revealed 0)
    (define/obs @draw-selection #f)
    (define/obs @discard-selection #f)
    (define @rows (obs-combine preview-rows @ability-deck @revealed))
    (define @discard-rows (@> @ability-deck make-discard-rows))
    ;; not setting current renderer, nor using an eventspace: dialog
    (render
     (dialog
      #:size '(600 400)
      #:style '(close-button resize-border)
      #:title "Ability Deck Previewer"
      (vpanel
       ;; draw
       (group
        "Ability Card Draw Pile"
        (hpanel
         (table '("Ability Card")
                @rows
                (match-lambda**
                  [{'select _ selection}
                   (:= @draw-selection selection)]
                  [{_ _ _} (void)])
                #:min-size '(200 #f))
         (revealed-ability-card-information @ability-deck @draw-selection @revealed @mg @env)
         ;; reveal draw
         (vpanel
          (button "Reveal 1" (thunk (<@ @revealed {switch [number? add1]}))
                  #:enabled? (obs-combine
                              {(~> (== _ (~> ability-decks-draw length))
                                   (and (~> 1> number?) <))}
                              @revealed @ability-deck))
          (button "Reveal All" (thunk (:= @revealed 'all))
                  #:enabled? (@> @revealed number?))
          (button "Move Top Card to Bottom"
                  (thunk
                   (on-move)
                   (<@ @revealed {switch [number? sub1]}))))))
       ;; discard
       (group
        "Ability Card Discard Pile"
        (hpanel
         (table '("Ability Card")
                @discard-rows
                (match-lambda**
                  [{'select _ selection}
                   (:= @discard-selection selection)]
                  [{_ _ _} (void)]))
         (discard-ability-card-information @ability-deck @discard-selection @mg @env)))))))))

(define (preview-rows ability-deck revealed)
  (define draw (ability-decks-draw ability-deck))
  (define reveal {~> monster-ability-name->text vector})
  (define hide {(gen (vector "?"))})
  (make-preview-rows draw revealed #:reveal reveal #:hide hide))

(define (update-selected-tracker e tracker)
  (match e
    [`(include? ,num to #t) (hash-update tracker num values #f)]
    [`(include? ,num to #f) (hash-remove tracker num)]
    [`(elite? ,num to ,elite?)
     ;; looks like hash-set, but I want the missing-key semantics of
     ;; hash-update with no failure-result as a guard against bugs
     (hash-update tracker num (const elite?))]))

(module+ test
  (test-case "update-selected-tracker"
    (check-equal? (update-selected-tracker '(include? 3 to #t) (hash))
                  (hash 3 #f))
    (check-equal? (update-selected-tracker '(include? 3 to #t) (hash 3 #f))
                  (hash 3 #f))
    (check-equal? (update-selected-tracker '(include? 3 to #t) (hash 4 #f))
                  (hash 3 #f 4 #f))
    (check-equal? (update-selected-tracker '(include? 3 to #f) (hash))
                  (hash))
    (check-equal? (update-selected-tracker '(include? 3 to #f) (hash 3 #f))
                  (hash))
    (check-equal? (update-selected-tracker '(include? 3 to #f) (hash 4 #f))
                  (hash 4 #f))
    (check-equal? (update-selected-tracker '(include? 3 to #f) (hash 3 #f 4 #f))
                  (hash 4 #f))
    (check-equal? (update-selected-tracker '(elite? 3 to #t) (hash 3 #f 4 #t))
                  (hash 3 #t 4 #t))
    (check-equal? (update-selected-tracker '(elite? 3 to #f) (hash 3 #t 4 #t))
                  (hash 3 #f 4 #t))
    (check-equal? (update-selected-tracker '(elite? 3 to #t) (hash 3 #t 4 #t))
                  (hash 3 #t 4 #t))
    (check-equal? (update-selected-tracker '(elite? 3 to #f) (hash 3 #f 4 #t))
                  (hash 3 #f 4 #t))
    (check-exn exn:fail? (thunk (update-selected-tracker '(elite? 3 to #t) (hash 4 #t))))
    (check-exn exn:fail? (thunk (update-selected-tracker '(elite? 3 to #f) (hash 4 #t))))))

(define (vector-update! v pos f)
  (vector-set! v pos (f (vector-ref v pos))))

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
              [`(set from ,_old to ,new) (<@ @state {(list-set 0 new)})]
              [`(monster from ,_old to ,new) (<@ @state {(list-set 1 new)})]
              [`(include? ,n to #t)
                (<@ @state {(list-update 2 {(hash-update n values #f)})})]
              [`(include? ,n to #f)
                (<@ @state {(list-update 2 {(hash-remove n)})})]
              [`(elite? ,n to ,elite?)
                ;; looks like hash-set, but I want the missing-key semantics of
                ;; hash-update with no failure-result as a guard against bugs
                (<@ @state {(list-update 2 {(hash-update n (const elite?))})})])))))

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
              (define/obs @deck (ability-decks #f (shuffle abilities-for-group) empty))
              (define/obs @draw? #t)
              (define/match (swap _who)
                [{'all} (<@ @mg swap-monster-group-elites)]
                [{n} (<@ @mg (monster-group-update-num n swap-monster-elite))])
              (with-closing-custodian/eventspace
                (render/eventspace
                  #:eventspace closing-eventspace
                  (window
                    #:title "Monster view"
                    #:mixin close-custodian-mixin
                    (monster-group-view
                      @mg @deck @n (@ #hash())
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
                      #:on-select (λ:= @n)
                      #:on-swap swap
                      #:on-move-ability-card (thunk (<@ @deck move-top-draw-to-bottom))
                      #:on-max-hp (λ (f) (<@ @mg {(monster-group-change-max-HP f (hash))}))
                      #:on-update (λ (f) (<@ @mg f)))
                    (hpanel
                      (button
                        "Draw"
                        (thunk
                         (<@ @deck ability-decks-draw-next)
                         (<@ @draw? not))
                        #:enabled? @draw?)
                      (button
                        "Next Round"
                        (thunk
                         (<@ @deck ability-decks-discard-and-maybe-shuffle)
                         (<@ @draw? not))
                        #:enabled? (@> @draw? not))))))]
            [_ (void)]))))))
