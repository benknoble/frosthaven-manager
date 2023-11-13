#lang racket

(provide
 (contract-out
  [player-rewards-view (->* ((obs/c num-players/c)
                             (obs/c level/c)
                             (obs/c (listof player?)))
                            (#:mixin (make-mixin-contract top-level-window<%>))
                            (is-a?/c view<%>))]))

(require (only-in racket/gui top-level-window<%>)
         racket/gui/easy
         racket/gui/easy/contract
         frosthaven-manager/defns
         frosthaven-manager/observable-operator)

(module+ test (require rackunit))

(define labels
  (append (list "Player" "Random Item?" "XP" "Gold")
          (map ~a material-kinds)
          (map ~a herb-kinds)))

(define (player-rewards-view @num-players @level @players #:mixin [mix values])
  (define @entries (obs-combine make-entries @players @num-players @level))
  (define/obs @selection #f)
  (define/match (update-selection _action _entries _selection)
    [{'select _ selection} (:= @selection selection)]
    [{_ _ _} (void)])
  (define @selected-entry (obs-combine make-selected-entry @entries @selection))
  (window
   #:mixin mix
   #:title "Loot and XP"
   #:size '(400 300)
   (loot-and-xp-view @entries update-selection @selection)
   (loot-cards-view @selected-entry)))

(define (loot-and-xp-view @entries action @selection)
  (table labels
         @entries
         action
         #:entry->row entry->row
         #:selection @selection
         #:column-widths
         (for/list ([(label i) (in-indexed (in-list labels))])
           (list i (* 10 (string-length label))))
         #:min-size '(400 150)))

(define (loot-cards-view @entry)
  (input
   (@> @entry entry-loot-text)
   #:enabled? #f
   #:style '(multiple)
   #:min-size '(400 150)))

(define (entry->row e)
  (match-define (list p num-players level) e)
  (define loots (player-loot p))
  (apply vector
         (player-name p)
         (if (memf random-item? loots) "x" "")
         (~a (player-xp p))
         (~a (for/sum ([loot (in-list loots)] #:when (money? loot))
               (* (money-amount loot)
                  (level-info-gold (get-level-info level)))))
         (append
          (for/list ([material material-kinds])
            (~a (for/sum ([loot (in-list loots)]
                          #:when (and (material? loot)
                                      (equal? material (material-name loot))))
                  (list-ref (material-amount loot) (- num-players 2)))))
          (for/list ([herb herb-kinds])
            (~a (for/sum ([loot (in-list loots)]
                          #:when (and (herb? loot)
                                      (equal? herb (herb-name loot))))
                  (herb-amount loot)))))))

(module+ test
  (test-case "entry->row"
    (define num-players 2)
    (define level 3) ;; gold conversion is 3
    (define base-p (make-player "Jeff" 1))
    (define (actual p)
      (entry->row (list p num-players level)))
    (check-equal? (actual base-p)
                  #("Jeff" "" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0"))
    (define (loot p card)
      ((player-add-loot card) p))
    (define (loot-test card)
      (loot base-p card))
    (check-equal? (actual (loot-test random-item))
                  #("Jeff" "x" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0"))
    (check-equal? (actual ((player-act-on-xp (const 5)) base-p))
                  #("Jeff" "" "5" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0"))
    (check-equal? (actual (loot-test (money 2)))
                  #("Jeff" "" "0" "6" "0" "0" "0" "0" "0" "0" "0" "0" "0"))
    (check-equal? (actual (loot-test (material lumber '(3 2 1))))
                  #("Jeff" "" "0" "0" "3" "0" "0" "0" "0" "0" "0" "0" "0"))
    (check-equal? (actual (loot-test (material metal '(3 2 1))))
                  #("Jeff" "" "0" "0" "0" "3" "0" "0" "0" "0" "0" "0" "0"))
    (check-equal? (actual (loot-test (material hide '(3 2 1))))
                  #("Jeff" "" "0" "0" "0" "0" "3" "0" "0" "0" "0" "0" "0"))
    (check-equal? (actual (loot-test (herb arrowvine 1)))
                  #("Jeff" "" "0" "0" "0" "0" "0" "1" "0" "0" "0" "0" "0"))
    (check-equal? (actual (loot-test (herb axenut 1)))
                  #("Jeff" "" "0" "0" "0" "0" "0" "0" "1" "0" "0" "0" "0"))
    (check-equal? (actual (loot-test (herb corpsecap 1)))
                  #("Jeff" "" "0" "0" "0" "0" "0" "0" "0" "1" "0" "0" "0"))
    (check-equal? (actual (loot-test (herb flamefruit 1)))
                  #("Jeff" "" "0" "0" "0" "0" "0" "0" "0" "0" "1" "0" "0"))
    (check-equal? (actual (loot-test (herb rockroot 1)))
                  #("Jeff" "" "0" "0" "0" "0" "0" "0" "0" "0" "0" "1" "0"))
    (check-equal? (actual (loot-test (herb snowthistle 1)))
                  #("Jeff" "" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "1"))
    (check-equal? (~> (base-p)
                      (loot (material lumber '(2 1 1)))
                      (loot (herb axenut 1))
                      (loot (material lumber '(2 1 1)))
                      (loot (herb arrowvine 3))
                      (loot (material lumber '(2 1 1)))
                      (loot (material lumber '(2 1 1)))
                      (loot (herb axenut 1))
                      actual)
                  #("Jeff" "" "0" "0" "8" "0" "0" "3" "2" "0" "0" "0" "0"))))

(define (make-entries players num-players level)
  (for/vector #:length (length players) ([p players])
    (list p num-players level)))

(define/match (make-selected-entry _entries _selection)
  [{_ #f} (list (make-player "" 1) 2 0)]
  [{entries selection} (vector-ref entries selection)])

(define/match (entry-loot-text _entry)
  [{(list (app player-loot loot-cards) num-players _)}
   (string-join (map (format-loot-card num-players) loot-cards) "\n")])
