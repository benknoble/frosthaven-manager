#lang racket

(provide
 (contract-out
  [struct monster-stats ([max-hp (or/c positive-integer? string?)]
                         [move (or/c #f natural-number/c)]
                         [attack (or/c natural-number/c string?)]
                         [bonuses (listof string?)]
                         [effects (listof string?)]
                         [immunities (listof string?)])]
  [struct monster-info ([set-name string?]
                        [name string?]
                        [normal-stats (apply list/c (build-list number-of-levels (const monster-stats?)))]
                        [elite-stats (apply list/c (build-list number-of-levels (const monster-stats?)))])]
  [struct monster-ability ([set-name string?]
                           [name string?]
                           [initiative initiative?]
                           [abilities (listof string?)]
                           [shuffle? boolean?]
                           [location (or/c path? #f)])]
  [monster-number/c contract?]
  [struct monster ([number monster-number/c]
                   [elite? boolean?]
                   [current-hp natural-number/c]
                   [conditions (listof condition?)])]
  [struct monster-group ([set-name string?]
                         [name string?]
                         [level level/c]
                         [normal-stats monster-stats?]
                         [elite-stats monster-stats?]
                         [monsters (listof monster?)])]
  [monster-stats-max-hp* (-> monster-stats? env/c natural-number/c)]
  [monster-stats-attack* (-> monster-stats? env/c natural-number/c)]
  [monster-stats-bonuses-string (-> monster-stats? string?)]
  [monster-stats-effects-string (-> monster-stats? string?)]
  [monster-stats-immunities-string (-> monster-stats? string?)]
  [monster-ability-name->text (-> (or/c #f monster-ability?) string?)]
  [monster-ability-initiative->text (-> (or/c #f monster-ability?) string?)]
  [monster-ability-ability->rich-text (-> string? (or/c #f monster-ability?)
                                          monster-group? env/c
                                          (listof (or/c string? pict:pict? pict/alt-text? newline?)))]
  [make-monster (-> monster-info? level/c
                    monster-number/c boolean?
                    env/c
                    monster?)]
  [make-monster-group (-> monster-info? level/c
                          (and/c (listof (cons/c monster-number/c boolean?))
                                 (unique-with/c car any/c))
                          env/c
                          monster-group?)]
  [get-monster-stats (-> monster-group? monster? monster-stats?)]
  [monster-at-max-health? (-> monster? monster-stats? env/c boolean?)]
  [monster-dead? (-> monster? boolean?)]
  [monster-group-update-num
   (-> monster-number/c
       (-> monster? monster?)
       (-> monster-group? monster-group?))]
  [monster-update-condition (-> condition? boolean?
                                (-> monster? monster?))]
  [monster-update-hp (-> (-> number? number?)
                         (-> monster? monster?))]
  [monster-group-remove (-> monster-number/c
                            (-> monster-group? monster-group?))]
  [monster-group-add (-> monster-number/c boolean? env/c
                         (-> monster-group? monster-group?))]
  [monster-group-first-monster (-> monster-group? (or/c #f monster-number/c))]
  [monster->hp-text (-> monster? monster-stats? env/c string?)]
  [swap-monster-group-elites (-> monster-group? monster-group?)]
  [swap-monster-elite (-> monster? monster?)]
  [monster-group-change-max-HP (-> monster-group? (-> (or/c 'normal 'elite) natural-number/c number?) env/c monster-group?)]))

(require
 racket/serialize
 (prefix-in pict: pict)
 frosthaven-manager/contracts
 frosthaven-manager/qi
 frosthaven-manager/parsers/formula
 frosthaven-manager/defns/level
 frosthaven-manager/defns/scenario
 (prefix-in elements: frosthaven-manager/elements)
 (submod frosthaven-manager/gui/rich-text-display model)
 (prefix-in icons: frosthaven-manager/icons))

(struct monster-stats [max-hp move attack bonuses effects immunities] #:prefab)
(struct monster-info [set-name name normal-stats elite-stats] #:prefab)
(struct monster-ability [set-name name initiative abilities shuffle? location] #:prefab)
(define monster-number/c (integer-in 1 10))
(serializable-struct monster [number elite? current-hp conditions] #:transparent)
(serializable-struct monster-group [set-name name level normal-stats elite-stats monsters] #:transparent)

(define (monster-stats-max-hp* stats env)
  (match (monster-stats-max-hp stats)
    [(? number? x) x]
    [(? string? s)
     (match ((parse-expr s) env)
       [(? positive-integer? x) x]
       [x (raise-arguments-error 'monster-stats-max-hp*
                                 "Calculated max HP is not positive"
                                 "Max HP" x
                                 "formula" s
                                 "environment" env)])]))

(define (monster-stats-attack* stats env)
  (match (monster-stats-attack stats)
    [(? number? x) x]
    [(? string? s)
     (match ((parse-expr s) env)
       [(? positive-integer? x) x]
       [x (raise-arguments-error 'monster-stats-attack*
                                 "Calculated attack is not positive"
                                 "attack" x
                                 "formula" s
                                 "environment" env)])]))

(define (monster-stats-bonuses-string m)
  (~> (m) monster-stats-bonuses (string-join ", ")))

(define (monster-stats-effects-string m)
  (~> (m) monster-stats-effects (string-join ", ")))

(define (monster-stats-immunities-string m)
  (~> (m) monster-stats-immunities (string-join ", ")))

(define-flow (monster-ability-name->text ability)
  (if monster-ability?
    (~>> (-< monster-ability-name
             (~> (if monster-ability-shuffle? " (shuffle)" "")))
         (format "~a~a"))
    ""))

(define-flow (monster-ability-initiative->text ability)
  (if monster-ability? (~> monster-ability-initiative ~a) "??"))

(define (monster-ability-ability->rich-text ability-text ability-card mg env)
  (define bulleted '(#rx"^" "· "))
  (define attack
    (list #px"(.*)((?i:attack))\\s+([+-])(\\d+)"
          (skip-if-grant-or-control (keyword-sub (flow (monster-stats-attack* env)) mg))))
  (define move
    (list #px"(.*)((?i:move))\\s+([+-])(\\d+)"
          (skip-if-grant-or-control (keyword-sub monster-stats-move mg))))
  (define effects
    (list #px"(.*)((?i:attack) \\d+) \\(E:(\\d+)\\)"
          (skip-if-grant-or-control
           (λ (_match base-attack elite-value)
             (define-values (effects elite-effects)
               (~> (mg)
                   (-< monster-group-normal-stats monster-group-elite-stats)
                   (>< monster-stats-effects)))
             (define common-effects (set-intersect effects elite-effects))
             (define only-normal-effects (set-subtract effects common-effects))
             (define only-elite-effects (set-subtract elite-effects common-effects))
             (~a base-attack
                 (if (not (empty? only-normal-effects))
                   (format " (N:~a)" (string-join only-normal-effects ", "))
                   "")
                 (if (not (empty? only-elite-effects))
                   (format " (E:~a~a)" elite-value (string-join only-elite-effects ", " #:before-first ", "))
                   (format " (E:~a)" elite-value))
                 (if (not (empty? common-effects))
                   (string-join common-effects ", " #:before-first ", ")
                   ""))))))
  (define (splice-aoe x)
    (match x
      [(regexp #rx"^(.*)aoe\\(([^)]+)\\)(.*)$"
               (list _ prefix aoe suffix))
       (define base
         (switch (ability-card)
           [monster-ability? monster-ability-location]
           [else "."]))
       (define aoe-pict
         (~> (base aoe)
             build-path
             (switch
               [file-exists? (~> get-aoe apply)]
               [else (gen (pict:text "AoE File Not Found"))])))
       (list prefix newline aoe-pict newline suffix)]
      [x (list x)]))
  (define (infuse-wild x)
    (match x
      [(regexp #px"(.*)(?i:infuse)\\s*(?i:any)(?:\\s*element)?(.*)$"
               (list _ prefix suffix))
       (list prefix (elements:element-pics-infused (elements:wild)) suffix)]
      [x (list x)]))
  (define (infuse-element x)
    (match x
      [(regexp #px"^(.*)(?i:infuse)\\s*(?i:(fire|ice|air|earth|light|dark))\\s*(.*)$"
               (list _ prefix element suffix))
       (list prefix
             (elements:element-pics-infused (element->element-pics element))
             suffix)]
      [x (list x)]))
  (define (consume-wild x)
      (match x
        [(regexp #px"(.*)(?i:consume)\\s*(?i:any)(?:\\s*element)?(.*)$"
                 (list _ prefix suffix))
         (list prefix (elements:element-pics-consume (elements:wild)) suffix)]
        [x (list x)]))
  (define (consume-element x)
    (match x
      [(regexp #px"^(.*)(?i:consume)\\s*(?i:(fire|ice|air|earth|light|dark))\\s*(.*)$"
               (list _ prefix element suffix))
       (list prefix
             (elements:element-pics-consume (element->element-pics element))
             suffix)]
      [x (list x)]))
  (define (target x)
    (match x
      [(regexp #px"^(.*)(?i:target)(\\s*\\d+)(.*)$"
               (list _ prefix digit suffix))
       (list prefix (scale-icon (icons:target)) digit suffix)]
      [(regexp #px"^(.*)(?i:target)(\\s*(?i:all))(.*)$"
               (list _ prefix all suffix))
       (list prefix (scale-icon (icons:target)) all suffix)]
      [(regexp #px"^(.*)(\\+\\d+\\s*)(?i:target(?:s)?)(\\s*.*)$"
               (list _ prefix +target suffix))
       (list prefix +target (scale-icon (icons:target)) suffix)]
      [x (list x)]))
  (define replacements
    (list bulleted
          attack
          effects
          move))
  (define pict-replacements
    (list splice-aoe
          infuse-element
          infuse-wild
          consume-element
          consume-wild
          target))
  (for/fold ([result (list (regexp-replaces ability-text replacements))])
            ([pict-replacement (in-list pict-replacements)])
    (append-map (only-on-text pict-replacement) result)))

(module+ test
  (require rackunit)
  (define env (hash))
  (define get-dbs (dynamic-require 'frosthaven-manager/monster-db 'get-dbs))
  (match-define (list mg mg1 mg2 mg3)
    (match-let-values ([{info _} (get-dbs "../testfiles/sample-bestiary-import.rkt")])
      (map (λ (level)
             (make-monster-group (~> (info) (hash-ref "archer") (hash-ref "hynox archer"))
                                 level
                                 empty
                                 env))
           (list 0 1 2 3))))
  (define ability-card
    (match-let-values ([{_ ability} (get-dbs  "../testfiles/sample-bestiary-import.rkt")])
      (~> (ability) (hash-ref "archer") first)))
  (test-equal? "Simple Attack"
               (monster-ability-ability->rich-text "Attack +1" ability-card mg env)
               (list "· Attack 3 (E:4, wound)"))
  (test-equal? "Simple Attack 1"
               (monster-ability-ability->rich-text "Attack +1" ability-card mg1 env)
               (list "· Attack 4 (E:5), wound"))
  (test-equal? "Simple Attack 2"
               (monster-ability-ability->rich-text "Attack +1" ability-card mg2 env)
               (list "· Attack 5 (E:6, stun), wound"))
  (test-equal? "Simple Attack 3"
               (monster-ability-ability->rich-text "Attack +1" ability-card mg3 env)
               (list "· Attack 6 (N:muddle) (E:7, stun), wound"))
  (test-equal? "Attack, X"
               (monster-ability-ability->rich-text "Attack +1, Push 1" ability-card mg3 env)
               (list "· Attack 6 (N:muddle) (E:7, stun), wound, Push 1"))
  (test-equal? "Simple Move"
               (monster-ability-ability->rich-text "Move +1" ability-card mg env)
               (list "· Move 3 (E:3)"))
  (test-equal? "Granted Attack"
               (monster-ability-ability->rich-text "Grant Piranha: Attack +1" ability-card mg env)
               (list "· Grant Piranha: Attack +1"))
  (test-equal? "Granted Move"
               (monster-ability-ability->rich-text "Grant Piranha: Move +1" ability-card mg env)
               (list "· Grant Piranha: Move +1"))
  (test-equal? "Controlled Attack"
               (monster-ability-ability->rich-text "Control Enemy: Attack +1" ability-card mg env)
               (list "· Control Enemy: Attack +1"))
  (test-equal? "Controlled Move"
               (monster-ability-ability->rich-text "Control Enemy: Move +1" ability-card mg env)
               (list "· Control Enemy: Move +1")))

(define ((keyword-sub stats-f mg) _match word +- amount)
  (define op (eval (string->symbol +-) (make-base-namespace)))
  (define amount* (string->number amount))
  (define normal-base (stats-f (monster-group-normal-stats mg)))
  (define elite-base (stats-f (monster-group-elite-stats mg)))
  (define normal (if normal-base (op normal-base amount*) "-"))
  (define elite (if elite-base (op elite-base amount*) "-"))
  (format "~a ~a (E:~a)" word normal elite))

(define ((skip-if-grant-or-control f) match before . args)
  (if (regexp-match #px"(?i:grant)|(?i:control)" before)
    match
    (format "~a~a" before (apply f (substring match (string-length before)) args))))

(define (not-an-aoe)
  (pict:text "Not an AoE module"))

(define (get-aoe path)
  (namespace-call-with-registry-lock
   (current-namespace)
   (thunk
    (dynamic-require path 'aoe (thunk not-an-aoe)))))

(define ((only-on-text f) x)
  (cond
    [(string? x) (f x)]
    [else (list x)]))

(define (element->element-pics e)
  ((case (string-downcase e)
     [("fire") elements:fire]
     [("ice") elements:ice]
     [("air") elements:air]
     [("earth") elements:earth]
     [("light") elements:light]
     [("dark") elements:dark])))

(define (make-monster* stats number elite? env)
  (monster number elite? (monster-stats-max-hp* stats env) empty))

(define (make-monster info level number elite? env)
  (define level-stats
    (list-ref (if elite?
                (monster-info-elite-stats info)
                (monster-info-normal-stats info))
              level))
  (make-monster* level-stats number elite? env))

(define (make-monster-group info level num+elite?s env)
  (define-values (normal elite)
    (~> (info)
        (-< monster-info-normal-stats
            monster-info-elite-stats)
        (amp (list-ref level))))
  (monster-group
    (monster-info-set-name info)
    (monster-info-name info)
    level
    normal elite
    (sort-monsters
      (map (match-lambda
             [(cons num elite?)
              (make-monster* (if elite? elite normal) num elite? env)])
           num+elite?s))))

(define-switch (get-monster-stats mg m)
  (% 2> 1>)
  [monster-elite? monster-group-elite-stats]
  [else monster-group-normal-stats])

(define-flow (monster-at-max-health? m stats env)
  (~> (group 1 monster-current-hp monster-stats-max-hp*) >=))

(define-flow (monster-dead? m)
  (~> monster-current-hp zero?))

(define-flow (sort-monsters monsters)
  ;; two passes less efficient, but easier to reason about AND we expect most
  ;; monsters lists to be "short" (10 or less).
  (~> (sort #:key monster-number <)
      ;; Notation: use t and f for #true and #false.
      ;; truth-table for strict sort-by-monster-elite?
      ;; a | b | a is first?
      ;; --+---+------------
      ;; t | t | equal ∴ f
      ;; t | f | t
      ;; f | t | f
      ;; f | f | equal ∴ f
      ;; On xor: (xor a b) is true ⇔ a and b are different. By itself, this already
      ;; covers the first and last rows of the table. The second row is also
      ;; correct, but the third is wrong. Notice that and'ing the result with a will
      ;; produce the final truth-table (since ∀ x boolean, (and x #f) = #f and also
      ;; (and x #t) = x).
      (sort #:key monster-elite? (λ (a b) (and (xor a b) a)))))

(module+ test
  (require rackunit)
  (test-case
    "sort-monsters groups by elite? and sorts by number"
    (check-equal? (sort-monsters (list (monster 1 #f 0 empty)
                                       (monster 2 #t 0 empty)))
                  (list (monster 2 #t 0 empty) (monster 1 #f 0 empty)))
    (check-equal?
      (sort-monsters
        (list
          (monster 4 #f 0 empty)
          (monster 2 #t 0 empty)
          (monster 3 #f 0 empty)
          (monster 1 #t 0 empty)))
      (list
        (monster 1 #t 0 empty)
        (monster 2 #t 0 empty)
        (monster 3 #f 0 empty)
        (monster 4 #f 0 empty)))))

(define ((monster-group-update-num num f) group)
  ;; TODO: lenses?
  ;; TODO: should monster-group-monsters be a hash?
  ;; current contract doesn't even have uniqueness
  (define (is-num? m) (= num (monster-number m)))
  (define old-monsters (monster-group-monsters group))
  (define the-monster (findf is-num? old-monsters))
  (define new-monster (f the-monster))
  (define new-monsters
    (sort-monsters (cons new-monster (remove the-monster old-monsters))))
  (struct-copy monster-group group [monsters new-monsters]))

(define ((monster-group-remove num) group)
  (define (is-num? m) (= num (monster-number m)))
  (define old-monsters (monster-group-monsters group))
  (define the-monster (findf is-num? old-monsters))
  (define new-monsters
    (sort-monsters (remove the-monster old-monsters)))
  (struct-copy monster-group group [monsters new-monsters]))

(define ((monster-group-add num elite? env) group)
  (when (~>> (group) monster-group-monsters (map monster-number) (member num))
    (raise-arguments-error 'monster-group-add
                           (format "Monster ~a already exists in group" num)
                           "num" num
                           "group" group))
  (define new-monster
    (make-monster* (if elite?
                     (monster-group-elite-stats group)
                     (monster-group-normal-stats group))
                   num
                   elite?
                   env))
  (define new-monsters
    (sort-monsters (cons new-monster (monster-group-monsters group))))
  (struct-copy monster-group group [monsters new-monsters]))

(define-flow (monster-group-first-monster mg)
  (~> monster-group-monsters
      (and (not empty?) (~> first monster-number))))

(define ((monster-update-condition c on?) m)
  (define old-conditions (monster-conditions m))
  (define new-conditions
    (if on?
      (cons c (remove* (list c) old-conditions))
      (remove* (list c) old-conditions)))
  (struct-copy monster m [conditions new-conditions]))

(define ((monster-update-hp proc) m)
  (define old-hp (monster-current-hp m))
  (define new-hp (proc old-hp))
  (if (positive? new-hp)
    (struct-copy monster m [current-hp new-hp])
    m))

(define-flow (monster->hp-text m ms env)
  (~>> (group 1 monster-current-hp monster-stats-max-hp*)
       (format "HP: ~a/~a")))

(define (swap-monster-group-elites mg)
  (struct-copy monster-group mg
               [monsters (sort-monsters (map swap-monster-elite (monster-group-monsters mg)))]))

(define (swap-monster-elite m)
  (struct-copy monster m
               [elite? (not (monster-elite? m))]))

(define (monster-group-change-max-HP mg f env)
  (define (update-stats stats type)
    (define current-max (monster-stats-max-hp* stats env))
    (define new-max (f type current-max))
    (if (positive? new-max)
      (struct-copy monster-stats stats [max-hp new-max])
      stats))
  (struct-copy monster-group mg
               [normal-stats (update-stats (monster-group-normal-stats mg) 'normal)]
               [elite-stats (update-stats (monster-group-elite-stats mg) 'elite)]))
