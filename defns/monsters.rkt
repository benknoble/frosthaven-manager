#lang racket
; vim: lw+=match-loop

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
                        [normal-stats (apply list/c (make-list number-of-levels monster-stats?))]
                        [elite-stats (apply list/c (make-list number-of-levels monster-stats?))])]
  [struct monster-ability ([set-name string?]
                           [name string?]
                           [initiative initiative?]
                           ;; TODO: allow newlines, whole rich-text model?
                           [abilities (listof (listof (or/c string? pict:pict?)))]
                           [shuffle? boolean?])]
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
  [monster-ability-ability->rich-text (-> (listof (or/c string? pict:pict?))
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
  [monster-expire-conditions (-> monster? monster?)]
  [monster-update-hp (-> (-> number? number?)
                         (-> monster? monster?))]
  [monster-group-remove (-> monster-number/c
                            (-> monster-group? monster-group?))]
  [monster-group-add (-> monster-number/c boolean? env/c
                         (-> monster-group? monster-group?))]
  [monster-group-first-monster (-> monster-group? (or/c #f monster-number/c))]
  [monster-group-update-level (-> monster-group? monster-info? level/c monster-group?)]
  [monster->hp-text (-> monster? monster-stats? env/c string?)]
  [swap-monster-group-elites (-> monster-group? monster-group?)]
  [swap-monster-elite (-> monster? monster?)]
  [monster-group-change-max-HP (-> monster-group? (-> (or/c 'normal 'elite) natural-number/c number?) env/c monster-group?)]))

(require
 racket/serialize
 (prefix-in pict: pict)
 frosthaven-manager/contracts
 frosthaven-manager/curlique
 frosthaven-manager/parsers/formula
 frosthaven-manager/defns/level
 frosthaven-manager/defns/scenario
 (prefix-in elements: frosthaven-manager/elements)
 (submod frosthaven-manager/gui/rich-text-display model)
 (prefix-in icons: frosthaven-manager/icons)
 frosthaven-manager/rich-text-helpers)

(struct monster-stats [max-hp move attack bonuses effects immunities] #:prefab)
(struct monster-info [set-name name normal-stats elite-stats] #:prefab)
(struct monster-ability [set-name name initiative abilities shuffle?] #:prefab)
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

(define monster-stats-bonuses-string
  {~> monster-stats-bonuses (string-join ", ")})

(define monster-stats-effects-string
  {~> monster-stats-effects (string-join ", ")})

(define monster-stats-immunities-string
  {~> monster-stats-immunities (string-join ", ")})

(define-flow (monster-ability-name->text _ability)
  (if monster-ability?
    (~>> (-< monster-ability-name
             (~> (if monster-ability-shuffle? " (shuffle)" "")))
         (format "~a~a"))
    ""))

(define-flow (monster-ability-initiative->text _ability)
  (if monster-ability? (~> monster-ability-initiative ~a) "??"))

(define (monster-ability-ability->rich-text ability-parts mg env)
  (define part->rich-text
    (only-on-text
     {(monster-ability-part->rich-text mg env)}))
  (~>> (ability-parts)
       ;; bestiary expander produces some empty suffix strings
       (filter {(not (equal? ""))})
       ;; only the first item in each ability gets a bullet
       make-first-bullet
       ;; everything else has a hard newline (typically only split on images,
       ;; though)
       (add-between _ newline)
       (append-map part->rich-text)))

(define (make-first-bullet ability-parts)
  (match ability-parts
    [(cons (? string? first) rest)
     (cons (regexp-replaces first '([#rx"^" "• "])) rest)]
    [(cons x xs) (list* "• " x xs)]
    [_ ability-parts]))

(define (monster-ability-part->rich-text* ability-text mg env)
  (define attack
    (list #px"(.*)((?i:attack))\\s+([+-])(\\d+)"
          (skip-if-grant-or-control (keyword-sub {(monster-stats-attack* env)} mg))))
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
  (define (infuse-wild x)
    (match-loop x
      [(regexp #px"(.*)(?i:infuse)\\s*(?i:any)(?:\\s*element)?(.*)$"
               (list _ prefix suffix))
       (list prefix (elements:element-pics-infused (elements:wild)) suffix)]))
  (define (infuse-element x)
    (match-loop x
      [(regexp #px"^(.*)(?i:infuse)\\s*(?i:(fire|ice|air|earth|light|darkness|dark))((?:\\s*,\\s*(?i:(?:fire|ice|air|earth|light|darkness|dark)))*)\\s*(.*)$"
               (list _ prefix element more-elements? suffix))
       (append (list prefix
                     (elements:element-pics-infused (element->element-pics element)))
               (map {~> element->element-pics elements:element-pics-infused}
                    (regexp-match* #px"(?i:fire|ice|air|earth|light|darkness|dark)" more-elements?))
               (list suffix))]))
  (define (consume-wild x)
      (match-loop x
        [(regexp #px"(.*)(?i:consume)\\s*(?i:any)(?:\\s*element)?(.*)$"
                 (list _ prefix suffix))
         (list prefix (elements:element-pics-consume (elements:wild)) suffix)]))
  (define (consume-element x)
    (match-loop x
      [(regexp #px"^(.*)(?i:consume)\\s*(?i:(fire|ice|air|earth|light|darkness|dark))\\s*(.*)$"
               (list _ prefix element suffix))
       (list prefix
             (elements:element-pics-consume (element->element-pics element))
             suffix)]))
  (define (target x)
    (match-loop x
      [(regexp #px"^(.*)(?i:target)(\\s*\\d+)(.*)$"
               (list _ prefix digit suffix))
       (list prefix (scale-icon (icons:target)) digit suffix)]
      [(regexp #px"^(.*)(?i:target)(\\s*(?i:all))(.*)$"
               (list _ prefix all suffix))
       (list prefix (scale-icon (icons:target)) all suffix)]
      [(regexp #px"^(.*)(\\+\\d+\\s*)(?i:target(?:s)?)(\\s*.*)$"
               (list _ prefix +target suffix))
       (list prefix +target (scale-icon (icons:target)) suffix)]))
  (define (range x)
    (match-loop x
      [(regexp #px"^(.*)(?i:range)(.*)$"
               (list _ prefix suffix))
       (list prefix (scale-icon (icons:range)) suffix)]))
  (define (push-pull x)
    (match-loop x
      [(regexp #px"^(.*)((?i:push)|(?i:pull))(.*)$"
               (list _ prefix push-pull suffix))
       (list prefix
             (scale-icon
              (match (string-downcase push-pull)
                ["push" (icons:push)]
                ["pull" (icons:pull)]))
             suffix)]))
  (define (move-icon x)
    (match-loop x
      [(regexp #px"^(.*)(?i:move)(.*)$"
               (list _ prefix suffix))
       (list prefix (scale-icon (icons:move)) suffix)]))
  (define (jump x)
    (match-loop x
      [(regexp #px"^(.*)(?i:jump)(.*)$"
               (list _ prefix suffix))
       ;; NOTE unscaled: this icon is a reasonable size and is hard to make
       ;; visually useful when scaled down.
       (list prefix (icons:jump) suffix)]))
  (define (teleport x)
    (match-loop x
      [(regexp #px"^(.*)(?i:teleport)(.*)$"
               (list _ prefix suffix))
       ;; NOTE unscaled
       (list prefix (icons:teleport) suffix)]))
  (define (attack-icon x)
    (match-loop x
      [(regexp #px"^(.*)(?i:attack)(\\s+[+-]?\\d+.*)$"
               (list _ prefix suffix))
       ;; NOTE unscaled
       (list prefix (icons:attack) suffix)]))
  (define replacements
    (list attack
          effects
          move))
  (define pict-replacements
    (list infuse-element
          infuse-wild
          consume-element
          consume-wild
          target
          range
          push-pull
          move-icon
          jump
          teleport
          attack-icon))
  (for/fold ([result (list (regexp-replaces ability-text replacements))])
            ([pict-replacement (in-list pict-replacements)])
    (append-map (only-on-text pict-replacement) result)))

;; highly specific to the monster-ability-part->rich-text!
(define (memoize f)
  (define cache (make-hash))
  (define-syntax-rule (hash [k v] ...)
    (make-hash (list (cons k v) ...)))
  (define-syntax-rule (mg-key mg)
    (list (monster-group-normal-stats mg)
          (monster-group-elite-stats mg)))
  (λ (ability-text mg env)
    (~> (cache)
        (hash-ref! ability-text (thunk (hash [(mg-key mg) (hash [env (f ability-text mg env)])])))
        (hash-ref! (mg-key mg)  (thunk                    (hash [env (f ability-text mg env)])))
        (hash-ref! env          (thunk                               (f ability-text mg env))))))

(define monster-ability-part->rich-text (memoize monster-ability-part->rich-text*))

(module+ test
  (require rackunit)
  (define env (hash))
  (define get-dbs (dynamic-require 'frosthaven-manager/monster-db 'get-dbs))
  (match-define (list mg mg1 mg2 mg3)
    (match-let-values ([{info _} (get-dbs "../testfiles/sample-bestiary-import.rkt")])
      (for/list ([level '(0 1 2 3)])
        (make-monster-group (~> (info) (hash-ref "archer") (hash-ref "hynox archer"))
                            level
                            empty
                            env))))
  (define model-equal? (dynamic-require '(submod frosthaven-manager/gui/rich-text-display model) 'model-equal?))
  (define-binary-check (check-model-equal? model-equal? _actual _expected))
  (define-syntax-rule (test-model-equal? name actual expected)
    (test-check name check-model-equal? actual expected))
  (test-model-equal? "Simple Attack (and empty strings filtered)"
                     (monster-ability-ability->rich-text (list "Attack +1" "") mg env)
                     (list "• " (icons:attack) " 3 (E:4, wound)"))
  (test-model-equal? "Simple Attack 1"
                     (monster-ability-ability->rich-text (list "Attack +1") mg1 env)
                     (list "• " (icons:attack) " 4 (E:5), wound"))
  (test-model-equal? "Simple Attack 2"
                     (monster-ability-ability->rich-text (list "Attack +1") mg2 env)
                     (list "• " (icons:attack) " 5 (E:6, stun), wound"))
  (test-model-equal? "Simple Attack 3"
                     (monster-ability-ability->rich-text (list "Attack +1") mg3 env)
                     (list "• " (icons:attack) " 6 (N:muddle) (E:7, stun), wound"))
  (test-model-equal? "Attack, X"
                     (monster-ability-ability->rich-text (list "Attack +1, Push 1") mg3 env)
                     (list "• " (icons:attack) " 6 (N:muddle) (E:7, stun), wound, " (scale-icon (icons:push)) " 1"))
  (test-model-equal? "Simple Move"
                     (monster-ability-ability->rich-text (list "Move +1") mg env)
                     (list "• " (scale-icon (icons:move)) " 3 (E:3)"))
  (test-model-equal? "Granted Attack"
                     (monster-ability-ability->rich-text (list "Grant Piranha: Attack +1") mg env)
                     (list "• Grant Piranha: " (icons:attack) " +1"))
  (test-model-equal? "Granted Move"
                     (monster-ability-ability->rich-text (list "Grant Piranha: Move +1") mg env)
                     (list "• Grant Piranha: " (scale-icon (icons:move)) " +1"))
  (test-model-equal? "Controlled Attack"
                     (monster-ability-ability->rich-text (list "Control Enemy: Attack +1") mg env)
                     (list "• Control Enemy: " (icons:attack) " +1"))
  (test-model-equal? "Controlled Move"
                     (monster-ability-ability->rich-text (list "Control Enemy: Move +1") mg env)
                     (list "• Control Enemy: " (scale-icon (icons:move)) " +1"))
  (test-model-equal? "Complicated Targeting"
                     (monster-ability-ability->rich-text (list "Attack -2, Target 2, +2 Targets, Range 3, Push 2") mg env)
                     (list
                      "• " (icons:attack) " 0 (E:1, wound), "
                      (scale-icon (icons:target)) " 2"
                      ", " "+2 " (scale-icon (icons:target)) ", "
                      (scale-icon (icons:range)) " 3, "
                      (scale-icon (icons:push)) " 2"))
  (test-model-equal? "Expander result"
                     (monster-ability-ability->rich-text (list "Attack +1" (pict:text "AoE") "and more")
                                                         mg env)
                     (list "• " (icons:attack) " 3 (E:4, wound)"
                           newline
                           (pict:text "AoE")
                           newline
                           "and more"))
  (test-model-equal? "Make first bullet"
                     (monster-ability-ability->rich-text (list (pict:text "AoE"))
                                                         mg env)
                     (list "• " newline (pict:text "AoE"))))

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

(define (element->element-pics e)
  ((case (string-downcase e)
     [("fire") elements:fire]
     [("ice") elements:ice]
     [("air") elements:air]
     [("earth") elements:earth]
     [("light") elements:light]
     [("dark") elements:dark]
     [("darkness") elements:dark])))

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

(define-switch (get-monster-stats _mg _m)
  (% 2> 1>)
  [monster-elite? monster-group-elite-stats]
  [else monster-group-normal-stats])

(define-flow (monster-at-max-health? _m _stats _env)
  (~> (group 1 monster-current-hp monster-stats-max-hp*) >=))

(define monster-dead?
  {~> monster-current-hp zero?})

(define-flow (sort-monsters _monsters)
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

(define monster-group-first-monster
  {~> monster-group-monsters
      (and (not empty?) (~> first monster-number))})

(define (monster-group-update-level mg info new-level)
  (define-values (normal elite)
    (~> (info)
        (-< monster-info-normal-stats
            monster-info-elite-stats)
        (>< (list-ref new-level))))
  (struct-copy monster-group mg
               [level new-level]
               [normal-stats normal]
               [elite-stats elite]))

(define ((monster-update-condition c on?) m)
  (define old-conditions (monster-conditions m))
  (define new-conditions
    (if on?
      (cons c (remove* (list c) old-conditions))
      (remove* (list c) old-conditions)))
  (struct-copy monster m [conditions new-conditions]))

(define (monster-expire-conditions m)
  (for/fold ([m m])
            ([c (in-set expirable-conditions)])
    ((monster-update-condition c #f) m)))

(define ((monster-update-hp proc) m)
  (define old-hp (monster-current-hp m))
  (define new-hp (proc old-hp))
  (if (positive? new-hp)
    (struct-copy monster m [current-hp new-hp])
    m))

(define-flow (monster->hp-text _m _ms _env)
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

;; vim: lw+=match-loop
