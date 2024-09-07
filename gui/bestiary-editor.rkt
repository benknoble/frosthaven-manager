#lang racket

(module+ main (void (main)))

;; (module+ main
;;   (require racket/gui/easy/debugger)
;;   (start-debugger))

(require (only-in racket/gui
                  application-about-handler)
         racket/gui/easy
         (only-in pretty-expressive pretty-print)
         frosthaven-manager/observable-operator
         frosthaven-manager/curlique
         frosthaven-manager/qi/utils
         frosthaven-manager/defns
         frosthaven-manager/monster-db
         frosthaven-manager/parsers/monster
         frosthaven-manager/pp/bestiary
         frosthaven-manager/gui/common-menu
         frosthaven-manager/gui/mixins
         frosthaven-manager/gui/stacked-tables
         frosthaven-manager/gui/counter
         frosthaven-manager/files)

(define (main)
  (define/obs @info-db (hash))
  (define/obs @ability-db (hash))
  (define/obs @imports (list))
  (define/obs @current-file #f)
  (define/obs @previous-files empty)
  (define/obs @next-files empty)
  (define/obs @error-text "")
  (define (call-with-error-text th)
    (:= @error-text "")
    (with-handlers ([exn:fail? (λ (e) (:= @error-text (exn-message e)))])
      (th)))
  (define-syntax-rule (with-error-text e ...)
    (call-with-error-text (thunk e ...)))
  (define (open-bestiary)
    (with-error-text
     (define file (get-file/filter "Bestiary" '("Bestiary" "*.rkt")))
     (when file
       (open-file file @previous-files @current-file @next-files @info-db @ability-db @imports))))
  (define (save-bestiary)
    (with-error-text
     (define file (put-file/filter "Bestiary" '("Bestiary" "*.rkt")
                                   (~> (@current-file) @! path-only)
                                   (~> (@current-file) @! file-name-from-path)))
     (when file
       (call-with-output-file*
        file
        (λ (op)
          (pretty-print
           (pretty-bestiary (append (map {(list 'import _)} (@! @imports))
                                    (~> (@info-db) @! hash-values (append-map hash-values _))
                                    (~> (@ability-db) @! hash-values)))
           #:out op
           #:page-width 120))
        #:exists 'replace))))
  (define (remove-import)
    (define-close! close! closing-mixin)
    (define/obs @choice #f)
    (define (remove-import!)
      (when (@! @choice)
        (<@ @imports {(remove (@! @choice) _)}))
      (close!))
    ;; not setting current renderer, nor using an eventspace: dialog
    (render
     (dialog
      #:title "Remove Import"
      #:mixin closing-mixin
      (hpanel (choice @imports (λ:= @choice))
              (button "Remove" remove-import!))
      (button "Cancel" close!))))
  (define (edit . xs)
    (match (car xs)
      ['remove-monster (match-define (list monster-set monster-name) (cdr xs))
                       (<@ @info-db {(hash-update monster-set {(hash-remove monster-name)})})]
      ['stats
       (match (cdr xs)
         [(list* data f args)
          (when data
            (match-define (struct stats-editor-data [set name level elite? _info _name->info]) data)
            (define current-value (f (stats-editor-data-stats data)))
            (define (update-statss the-statss)
              (list-update
               the-statss
               level
               (λ (stats)
                 ;; TODO: should not cause contract violation
                 (match* (f args)
                   [{(== monster-stats-max-hp) (list proc)}
                    (struct-copy monster-stats stats [max-hp (proc current-value)])]
                   [{(== monster-stats-move) (list proc)}
                    (struct-copy monster-stats stats [move (proc current-value)])]
                   [{(== monster-stats-attack) (list proc)}
                    (struct-copy monster-stats stats [attack (proc current-value)])]
                   [{(== monster-stats-bonuses) '(new)}
                    (struct-copy monster-stats stats [bonuses (append current-value '(""))])]
                   [{(== monster-stats-bonuses) (list index 'remove)}
                    (struct-copy monster-stats stats [bonuses (list-remove current-value index)])]
                   [{(== monster-stats-bonuses) (list index value)}
                    (struct-copy monster-stats stats [bonuses (list-set current-value index value)])]
                   [{(== monster-stats-effects) '(new)}
                    (struct-copy monster-stats stats [effects (append current-value '(""))])]
                   [{(== monster-stats-effects) (list index 'remove)}
                    (struct-copy monster-stats stats [effects (list-remove current-value index)])]
                   [{(== monster-stats-effects) (list index value)}
                    (struct-copy monster-stats stats [effects (list-set current-value index value)])]
                   [{(== monster-stats-immunities) '(new)}
                    (struct-copy monster-stats stats [immunities (append current-value '(""))])]
                   [{(== monster-stats-immunities) (list index 'remove)}
                    (struct-copy monster-stats stats [immunities (list-remove current-value index)])]
                   [{(== monster-stats-immunities) (list index value)}
                    (struct-copy monster-stats stats [immunities (list-set current-value index value)])]))))
            (<@ @info-db
                (λ (info-db)
                  (hash-update
                   info-db
                   set
                   (λ (name->info)
                     (hash-update
                      name->info
                      name
                      (λ (info)
                        (define ->stats (if elite? monster-info-elite-stats monster-info-normal-stats))
                        (define the-statss (->stats info))
                        (if elite?
                          (struct-copy monster-info info [elite-stats (update-statss the-statss)])
                          (struct-copy monster-info info [normal-stats (update-statss the-statss)])))))))))])]
      ['abilities (void)]))
  (render
   (window
    #:title "Bestiary Editor"
    #:size '(800 600)
    (menu-bar
     (menu "File"
           (menu-item "&Open Bestiary" open-bestiary)
           (menu-item "&Save Bestiary" save-bestiary))
     (menu "Help"
           (about-menu-item)
           (menu-item-separator)
           (send-feedback-menu-item)
           (issue-menu-item)
           (feature-menu-item)
           (contribute-menu-item)))
    (hpanel (button "Back"
                    (back @previous-files @current-file @next-files @info-db @ability-db @imports)
                    #:enabled? (@> @previous-files {(not empty?)}))
            (input (@> @previous-files {(if empty? "(None)" (~> car ~a))})
                   #:enabled? #f
                   #:label "Back to:"))
    (hpanel (button "Forward"
                    (forward @previous-files @current-file @next-files @info-db @ability-db @imports)
                    #:enabled? (@> @next-files {(not empty?)}))
            (input (@> @next-files {(if empty? "(None)" (~> car ~a))})
                   #:enabled? #f
                   #:label "Forward to:"))
    (hpanel (spacer)
            (button "Open Bestiary" open-bestiary)
            (button "Save Bestiary" save-bestiary)
            (spacer))
    (input (@> @current-file {(if _ ~a "(None)")})
           #:enabled? #f
           #:label "Current File:")
    ;; imports-view
    (table
     '("Valid Import?" "Imports")
     (obs-combine
      (λ (imports current-file)
        (for/vector ([import imports])
          (vector (if (file-exists? (file-relative-to-current import current-file))
                    "✓"
                    "✗")
                  import)))
      @imports @current-file)
     (match-lambda**
       [{'dclick imports (? number? index)}
        (with-error-text
         (open-file (vector-ref (vector-ref imports index) 1) @previous-files @current-file @next-files @info-db @ability-db @imports))]
       [{_ _ _} (void)])
     #:min-size '(#f 150))
    (let ([@import (obs "")])
      (hpanel
       (input #:label "New Import:" @import (λ (_action inp) (:= @import inp)))
       (button "Import" (thunk
                         (<@ @imports {(cons (@! @import) _)})
                         (:= @import "")))))
    (button "Remove Import" remove-import)
    ;; db-view
    ;; TODO New Monster Buttons
    (bestiary-editor @info-db @ability-db edit)
    (cond-view
      [(@> @error-text non-empty-string?)
       (hpanel (text "Error message:" #:color "red")
               (input @error-text #:style '(multiple)))]
      [else (spacer)]))))

(define (bestiary-editor @info-db @ability-db edit)
  (define/obs @tab "Stats")
  (tabs '("Stats" "Abilities")
        #:selection @tab
        (λ (e _choices current)
          (case e [(select) (:= @tab current)]))
        (case-view @tab
          [("Stats") (stats-editor @info-db edit)]
          [("Abilities") (abilities-editor @ability-db edit)]
          [else (spacer)])))

(struct stats-editor-data [set name level elite? info name->info])

(define/match (stats-editor-data-stats _x)
  [{(stats-editor-data _ _ level elite? info _)}
   (define ->stats (if elite? monster-info-elite-stats monster-info-normal-stats))
   (~> (info) ->stats (list-ref level))])

(define (stats-editor @info-db edit)
  (apply stacked-tables
         ;; #(set)
         (@> @info-db {~> hash-keys (sort string<?) list->vector})
         (stats-editor-stats-editor edit)
         (stats-editor-columns @info-db)))

(define (stats-editor-columns @info-db)
  (list
   ;; set -> #((stats-editor-data set name _ _ _ name->info))
   (column "Set" values (λ (set)
                          (define name->info (hash-ref (@! @info-db) set))
                          (for/vector ([name (sort (hash-keys name->info) string<?)])
                            (stats-editor-data set name #f #f #f name->info))))
   ;; (stats-editor-data set name _ _ _ name->info) -> #((stats-editor-data set name level elite? info name->info))
   (column "Name" stats-editor-data-name (match-lambda
                                           [(stats-editor-data set name _ _ _ name->info)
                                            (define mi (hash-ref name->info name))
                                            (for*/vector ([level (in-range number-of-levels)]
                                                          [elite? (list #t #f)])
                                              (stats-editor-data set name level elite? mi name->info))]))
   ;; (stats-editor-data set name level elite? info name->info) -> (stats-editor-data …)
   (column "Level"
           (match-lambda [(stats-editor-data _ _ level elite? _ _)
                          (~a "Level " level (if elite? " (Elite)" ""))])
           values)))

;; (obs/c (or/c #f stats-editor-data?)) -> view
(define ((stats-editor-stats-editor edit) @data?)
  (define @stats? (@> @data? {(and _ stats-editor-data-stats)}))
  (apply vpanel
         (append (for/list ([part stats-editor-parts])
                   (match-define (list label f ed) part)
                   (ed label
                       (@> @stats? {(and _ f)})
                       (λ args (apply edit 'stats (@! @data?) f args))))
                 (list
                  (button "Delete Selected Monster"
                          (thunk
                           (define data (@! @data?))
                           (edit 'remove-monster (stats-editor-data-set data) (stats-editor-data-name data)))
                          #:enabled? @data?)))))

(define (abilities-editor @ability-db edit)
  ;; TODO
  (spacer))

(define (number-editor label @n edit)
  (counter (@> @n {~>> (or _ "-") (format "~a: ~a" label)})
           (thunk (edit add1))
           (thunk (edit sub1))))

(define (optional-number-editor label @n edit)
  (hpanel
   (button "-" (thunk (edit sub1)) #:enabled? @n)
   (text (@> @n {~>> (or _ "-") (format "~a: ~a" label)}))
   (button "+" (thunk (edit add1)) #:enabled? @n)
   (checkbox #:label (format "Has ~a" label)
             #:checked? @n
             (λ (on?) (edit (const (and on? 0)))))))

(define (list-editor label @xs edit)
  (define (start-edit @current-input @current-button @inputs @buttons)
    (for ([@input @inputs]) (:= @input #f))
    (for ([@button @buttons]) (:= @button #f))
    (:= @current-input #t)
    (:= @current-button #t))
  (define (finish-edit i value @inputs @buttons)
    (for ([@input @inputs]) (:= @input #f))
    (for ([@button @buttons]) (:= @button #t))
    (edit i value))
  (vpanel
   (text label)
   (observable-view
    @xs
    (λ (xs)
      (define @input-enableds
        ;; don't use const: we want separate observables
        (map (thunk* (@ #f)) (or xs empty)))
      (define @button-enableds
        ;; don't use const: we want separate observables
        (map (thunk* (@ #t)) (or xs empty)))
      (apply vpanel
             (for/list ([(x i) (in-indexed (or xs empty))]
                        [@input-enabled @input-enableds]
                        [@button-enabled @button-enableds])
               (define/obs @value x)
               (hpanel
                (button "Remove" (thunk (edit i 'remove)))
                (input x
                       (λ (action value)
                         (case action
                           [(input) (:= @value value)]
                           [(return) (finish-edit i value @input-enableds @button-enableds)]))
                       #:enabled? @input-enabled)
                (button (@> @input-enabled {(if _ "Save" "Edit")})
                        (thunk
                         (if (@! @input-enabled)
                           (finish-edit i (@! @value) @input-enableds @button-enableds)
                           (start-edit @input-enabled @button-enabled @input-enableds @button-enableds)))
                        #:enabled? @button-enabled))))))
   (button "+" (thunk (edit 'new)))))

(define stats-editor-parts
  `(["Max HP" ,monster-stats-max-hp ,number-editor]
    ["Move" ,monster-stats-move ,optional-number-editor]
    ["Attack" ,monster-stats-attack ,number-editor]
    ["Bonuses" ,monster-stats-bonuses ,list-editor]
    ["Effects" ,monster-stats-effects ,list-editor]
    ["Immunities" ,monster-stats-immunities ,list-editor]))

(define (open-file file @previous-files @current-file @next-files @info-db @ability-db @imports)
  (define current-file (@! @current-file))
  (define the-file (file-relative-to-current file current-file))
  (get-bestiary the-file @info-db @ability-db @imports)
  (when current-file
    (<@ @previous-files {(cons current-file _)}))
  (:= @current-file the-file)
  (:= @next-files empty))

(define ((back @previous-files @current-file @next-files @info-db @ability-db @imports))
  (define previous-file (@! (@> @previous-files {(if empty? #f first)})))
  (when previous-file
    (define current-file (@! @current-file))
    (<@ @next-files {(cons current-file _)})
    (:= @current-file previous-file)
    (<@ @previous-files rest)
    (get-bestiary previous-file @info-db @ability-db @imports)))

(define ((forward @previous-files @current-file @next-files @info-db @ability-db @imports))
  (define next-file (@! (@> @next-files {(if empty? #f first)})))
  (when next-file
    (define current-file (@! @current-file))
    (<@ @previous-files {(cons current-file _)})
    (:= @current-file next-file)
    (<@ @next-files rest)
    (get-bestiary next-file @info-db @ability-db @imports)))

(define (get-bestiary file @info-db @ability-db @imports)
  (define bestiary
    (call-with-input-file*
     file
     (λ (ip)
       (when (regexp-match-peek #rx"#lang" ip)
         (void (read-line ip 'any)))
       (parse-bestiary file ip #:syntax? #f))))
  (~> (bestiary)
      sep (>< (switch [(listof monster-ability?) sep])) collect
      datums->dbs
      (== (ε (:= @info-db _) ground)
          (ε (:= @ability-db _) ground)))
  (:= @imports (filter-map (and/c (list/c 'import string?)
                                  second)
                           bestiary)))

(define (file-relative-to-current file current-file)
  (cond
    [(absolute-path? file) file]
    [current-file
     (define base-dir (path-only current-file))
     (build-path base-dir file)]
    [else file]))
