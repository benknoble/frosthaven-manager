#lang racket

(provide
  (contract-out
    {struct column ([title string?]
                    [entry->label (-> any/c string?)]
                    [entry->next (-> any/c (or/c any/c (vectorof any/c)))])}
    (stacked-tables (->* ((obs/c (vectorof any/c))
                          (-> (obs/c (or/c #f any/c)) (is-a?/c view<%>))
                          column?)
                         (#:topleft? boolean?
                          #:panel (-> (is-a?/c view<%>) ... (is-a?/c view<%>)))
                         #:rest (listof column?)
                         (is-a?/c view<%>)))))

(require racket/gui/easy
         racket/gui/easy/contract
         frosthaven-manager/observable-operator)

(struct column [title entry->label entry->next])

(define (stacked-tables @data final-view
                        #:topleft? [topleft? #t]
                        #:panel [panel hpanel]
                        column1 . columns)
  (match-define (column title entry->label entry->next) column1)
  (define/obs @selection1 #f)
  (define t1
    (table (list title)
           @data
           (λ (_action _entries selection) (:= @selection1 selection))
           #:entry->row (flow (~> entry->label vector))
           #:selection @selection1))
  (define-values (r-tables last-@selection last-@data last-entry->next)
    (make-tables @data @selection1 entry->next t1 columns))
  (define @selected
    (obs-combine (flow (and in-vector-range (~> vector-ref last-entry->next)))
                 last-@data
                 last-@selection))
  (apply panel ((if topleft? reverse values) (cons (final-view @selected) r-tables))))

(define (make-tables @data @selection1 entry->next table1 columns)
  (for/fold ([r-tables (list table1)]
             [previous-@selection @selection1]
             [previous-@data @data]
             [previous-entry->next entry->next])
    ([col (in-list columns)])
    (match-define (column title entry->label entry->next) col)
    (define new-@data
      (obs-combine (flow (and in-vector-range
                              (~> vector-ref previous-entry->next ->vector)))
                   previous-@data
                   previous-@selection))
    (define/obs new-@selection #f)
    (define t
      (table (list title)
             (@~> new-@data (or _ (gen (vector))))
             (λ (_action _entries selection) (:= new-@selection selection))
             #:entry->row (flow (~> entry->label vector))
             #:selection new-@selection))
    (values (cons t r-tables) new-@selection new-@data entry->next)))

(define (in-vector-range v i)
  (and i v (< -1 i (vector-length v))))

(define-flow (->vector x) (if vector? _ vector))

(module* main racket
  (require (submod "..") ;; for contracts
           racket/gui/easy
           frosthaven-manager/gui/render
           frosthaven-manager/observable-operator)

  (require frosthaven-manager/monster-db
           frosthaven-manager/defns)
  (define-values (info-db _ability-db) (get-dbs default-monster-db))
  (render/eventspace
    (window
      (stacked-tables
        (~> (info-db) hash-keys list->vector @)
        (λ (@stats?) (text (@~> @stats? (if _ ~a "N/A"))))
        (column "Set" values (λ (set)
                               (define name->info (hash-ref info-db set))
                               (for/vector ([name (in-hash-keys name->info)])
                                 (list name set name->info))))
        (column "Name" first (match-lambda
                               [(list name set name->info)
                                (define mi (hash-ref name->info name))
                                (for*/vector ([level (in-range number-of-levels)]
                                              [elite? (list #t #f)])
                                  (list level elite? name set mi name->info))]))
        (column "Level"
                (match-lambda [(list* level elite? _) (~a "Level " level (if elite? " (Elite)" ""))])
                (match-lambda
                  [(list level elite? _name _set info _name->info)
                   (define ->stats (if elite? monster-info-elite-stats monster-info-normal-stats))
                   (~> (info) ->stats (list-ref level))]))
        ;; #:topleft? #f
        ;; #:panel vpanel
        ))))
