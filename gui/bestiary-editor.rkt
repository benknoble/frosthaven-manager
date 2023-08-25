#lang racket

;; TODO refactor duplicate code (gui/manager.rkt)

(module+ main (main))

(require (only-in racket/gui
                  get-file
                  put-file
                  application-about-handler)
         racket/gui/easy
         (only-in pretty-expressive pretty-print)
         frosthaven-manager/observable-operator
         frosthaven-manager/qi
         frosthaven-manager/defns
         frosthaven-manager/monster-db
         frosthaven-manager/parsers/monster
         frosthaven-manager/pp/bestiary
         frosthaven-manager/gui/monsters
         frosthaven-manager/gui/common-menu)

(define (main)
  (define/obs @info-db (hash))
  (define/obs @ability-db (hash))
  (define/obs @imports (list))
  (define/obs @current-file #f)
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
       (open-file file @current-file @info-db @ability-db @imports)
       (:= @current-file file))))
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
           (pretty-bestiary (append (map (flow (list 'import _)) (@! @imports))
                                    (~> (@info-db) @! hash-values (append-map hash-values _))
                                    (~> (@ability-db) @! hash-values)))
           #:out op
           #:page-width 120))
        #:exists 'replace))))
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
         (open-file (vector-ref (vector-ref imports index) 1) @current-file @info-db @ability-db @imports))]
       [{_ _ _} (void)]))
    ;; db-view
    ;; TODO Edit buttons
    ;; TODO New Set/Monster/Ability Buttons
    ;; requires some updates to db-view
    (db-view @info-db @ability-db (@ empty))
    (hpanel (button "Open Bestiary" open-bestiary)
            (button "Save Bestiary" save-bestiary))
    (cond-view
      [(@> @error-text non-empty-string?)
       (hpanel (text "Error message:" #:color "red")
               (input @error-text #:style '(multiple)))]
      [else (spacer)]))))

(define (open-file file @current-file @info-db @ability-db @imports)
  (define the-file
    (file-relative-to-current file (@! @current-file)))
  (define bestiary
    (call-with-input-file*
     the-file
     (λ (ip)
       (void (read-line ip)) ;; #lang line
       (parse-bestiary the-file ip #:syntax? #f))))
  (~> (bestiary)
      sep (>< (switch [(listof monster-ability?) sep])) collect
      datums->dbs
      (== (:= @info-db _)
          (:= @ability-db _)))
  (:= @imports (filter-map (and/c (list/c 'import string?)
                                  second)
                           bestiary))
  (:= @current-file the-file))

(define (file-relative-to-current file current-file)
  (cond
    [(absolute-path? file) file]
    [current-file
     (define base-dir (path-only current-file))
     (build-path base-dir file)]
    [else file]))

(define (get-file/filter message filter)
  (get-file message #f #f #f (->extension (second filter)) empty (list filter '("Any" "*.*"))))

(define (put-file/filter message filter [directory #f] [filename #f])
  (put-file message #f directory filename (->extension (second filter)) empty (list filter '("Any" "*.*"))))

(define-flow ->extension
  (~> path-get-extension (and _ (~> bytes->string/utf-8 (substring 1)))))
