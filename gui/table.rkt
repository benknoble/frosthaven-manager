#lang racket

(provide
 (contract-out
  [make-preview-rows (-> list?
                         (or/c 'all natural-number/c)
                         #:reveal (-> any/c (vectorof string?))
                         #:hide (-> any/c (vectorof string?))
                         (vectorof (vectorof string?)))]))

(require frosthaven-manager/curlique)

(module+ test (require rackunit))

(define (make-preview-rows xs n #:reveal reveal #:hide hide)
  (define-values {shown hidden}
    (match n
      ['all (values xs empty)]
      [(? number? n) (cond
                       [(<= 0 n (length xs)) (split-at xs n)]
                       [else (values xs empty)])]))
  (~> {shown hidden}
      (== (sep reveal) (sep hide))
      vector))

(module+ test
  (require frosthaven-manager/defns
           frosthaven-manager/manager/loot
           frosthaven-manager/monster-db)

  (test-case "make-preview-rows for monster abilities"
    (define-values {_info abilities} (get-dbs default-monster-db))
    (define in (shuffle (hash-ref abilities "archer")))
    (define name-text
      (list->vector (map vector (map monster-ability-name->text in))))
    (define reveal {~> monster-ability-name->text vector})
    (define hide {(gen (vector "?"))})
    (check-equal? (make-preview-rows in 0 #:reveal reveal #:hide hide)
                  (vector-map (const (vector "?")) name-text))
    (check-equal? (make-preview-rows in 5 #:reveal reveal #:hide hide)
                  (vector-append
                   (vector-take name-text 5)
                   (vector-map (const (vector "?")) (vector-drop name-text 5))))
    (check-equal? (make-preview-rows in 'all #:reveal reveal #:hide hide)
                  name-text)
    (check-equal? (make-preview-rows in (length in) #:reveal reveal #:hide hide)
                  name-text)
    (check-equal? (make-preview-rows in (add1 (length in)) #:reveal reveal #:hide hide)
                  name-text)
    (check-equal? (make-preview-rows in -5 #:reveal reveal #:hide hide)
                  name-text))

  (test-case "make-preview-rows for loot deck"
    (define loot-deck (build-loot-deck
                       (hash 'money 3
                             lumber 2
                             hide 2
                             axenut 2)
                       (hash 'money money-deck
                             lumber (hash-ref material-decks lumber)
                             hide (hash-ref material-decks hide)
                             axenut (hash-ref herb-decks axenut))))
    (define n-players 3)
    (define loot-text (list->vector (map vector (map (format-loot-card n-players) loot-deck))))
    (define reveal {~> (esc (format-loot-card n-players)) vector})
    (define hide-loot {(gen (vector "?"))})
    (check-equal? (make-preview-rows loot-deck 0 #:reveal reveal #:hide hide-loot)
                  (vector-map (const (vector "?")) loot-text))
    ;; check that vector-map didn't modify loot-text
    (check-equal? loot-text
                  (list->vector (map vector (map (format-loot-card n-players) loot-deck))))
    (check-equal? (make-preview-rows loot-deck 5 #:reveal reveal #:hide hide-loot)
                  (vector-append
                   (vector-take loot-text 5)
                   (vector-map (const (vector "?")) (vector-drop loot-text 5))))
    (check-equal? (make-preview-rows loot-deck 'all #:reveal reveal #:hide hide-loot)
                  loot-text)
    (check-equal? (make-preview-rows loot-deck (length loot-deck) #:reveal reveal #:hide hide-loot)
                  loot-text)
    (check-equal? (make-preview-rows loot-deck (add1 (length loot-deck)) #:reveal reveal #:hide hide-loot)
                  loot-text)
    (check-equal? (make-preview-rows loot-deck -5 #:reveal reveal #:hide hide-loot)
                  loot-text)))
