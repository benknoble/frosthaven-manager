#lang racket

(provide
 (contract-out
  [pretty-bestiary (->* (bestiary/c) (#:lang-line? any/c) doc?)]))

(require frosthaven-manager/curlique
         frosthaven-manager/defns
         frosthaven-manager/parsers/monster
         pretty-expressive
         (only-in frosthaven-manager/parsers/base name->set))

(define lang-line (text "#lang frosthaven-manager/bestiary"))

(define (pretty-bestiary bestiary #:lang-line? [lang-line? #t])
  (define-values (imports monster-infos monster-abilitiess)
    (~> (bestiary)
        sep
        (partition
         [(esc (list/c 'import string?)) collect]
         [monster-info? collect]
         [(esc (listof monster-ability?)) collect])))
  ;; TODO avoid spurious empty lines if preceding group is empty
  (<> (if lang-line? lang-line (<>))
      (pretty-imports imports)
      (pretty-monster-infos monster-infos)
      (pretty-monster-abilitiess monster-abilitiess)
      nl))

(define (pretty-imports imports)
  (empty-or imports
            (<> nl nl (v-concat (map pretty-import imports)))))

(define (pretty-import import)
  (match-define (list 'import file) import)
  (<s> (text "import-monsters") (text (~s file))))

(define (pretty-monster-infos monster-infos)
  (empty-or monster-infos
            (<> nl nl (v-concat
                       (add-between
                        (map pretty-monster-info monster-infos)
                        ;; NOTE: v-concat puts newlines around most elements, so
                        ;; instead of adding newlines add empty documents.
                        (<>))))))

(define (pretty-monster-info mi)
  (match-define (monster-info set-name name normal-stats elite-stats) mi)
  (<>
   (<> (text "begin-monster ")
       (text (~s name))
       (let ([computed-set-name (name->set name)])
         (cond
           [(equal? set-name computed-set-name) (<>)]
           [else (<> (text " ") lparen (text (~s set-name)) rparen)])))
   (nest 2 (<> nl (pretty-statss normal-stats elite-stats)))
   nl
   (text "end-monster")))

(define (pretty-statss normal-stats elite-stats)
  (define length-n/s
    {switch
      [false? 1]
      [number? (~> ~a string-length)]
      [string? (~> string-length (+ 2))]})
  (define max-hp-width
    (apply max (map {~> monster-stats-max-hp length-n/s}
                    (append normal-stats elite-stats))))
  (define max-move-width
    (apply max (map {~> monster-stats-move length-n/s}
                    (append normal-stats elite-stats))))
  (define max-attack-width
    (apply max (map {~> monster-stats-attack length-n/s}
                    (append normal-stats elite-stats))))
  (v-concat
   (append*
    (for/list ([(n-stat level) (in-indexed normal-stats)]
               [e-stat elite-stats])
      (list
       (pretty-stats "normal" level n-stat max-hp-width max-move-width max-attack-width)
       (pretty-stats "elite " level e-stat max-hp-width max-move-width max-attack-width))))))

(define (pretty-stats label level stats
                      max-hp-width max-move-width max-attack-width)
  (match-define (monster-stats max-hp move attack bonuses effects immunities) stats)
  (<+> lbrack
       (group
        (<$>
         (text (~a level))
         (text label)
         (group
          (<$>
           (labelled-value "HP" (aligned max-hp max-hp-width))
           (labelled-value "Move" (aligned (or move '-) max-move-width))
           (labelled-value "Attack" (aligned attack max-attack-width))))
         (group
          (<$>
           (empty-or bonuses (labelled-value "Bonuses" (pretty-list (map pretty-string bonuses))))
           (empty-or effects (labelled-value "Effects" (pretty-list (map pretty-string effects))))
           (empty-or immunities (labelled-value "Immunities" (pretty-list (map pretty-string immunities))))))))
       rbrack))

(define (pretty-monster-abilitiess monster-abilitiess)
  (empty-or monster-abilitiess
            (<> nl nl (v-concat
                       (add-between
                        (map pretty-ability-deck monster-abilitiess)
                        ;; NOTE: v-concat puts newlines around most elements, so
                        ;; instead of adding newlines add empty documents.
                        (<>))))))

(define (pretty-ability-deck ability-deck)
  (match-define (list (monster-ability set-name _ _ _ _ _) _ ...) ability-deck)
  (<>
   (<s> (text "begin-ability-deck") (text (~s set-name)))
   (nest 2 (<> nl (pretty-abilities ability-deck)))
   nl
   (text "end-ability-deck")))

(define (pretty-abilities ability-deck)
  (v-concat (map pretty-ability ability-deck)))

(define (pretty-ability ability-card)
  (match-define (monster-ability _ name initiative abilities shuffle? _) ability-card)
  (labelled-value
   (~s name)
   (<s> (text (format "~a~a" initiative (if shuffle? " shuffle" "")))
        (pretty-list (map pretty-string abilities)))))

(define (labelled-value label value-doc)
  (<+> lbrack (text label) space value-doc rbrack))

(define (pretty-list docs)
  (<+> lbrace (group (v-concat docs)) rbrace))

(define (pretty-string x)
  (text (~s x)))

(define (aligned x m)
  (text (~s x #:min-width m #:align 'right)))

(define (empty-or xs docs)
  (cond
    [(empty? xs) (<>)]
    [else docs]))

(module+ main
  (current-page-width 120)

  (define ip
    (command-line
     #:usage-help
     "Formats bestiary files."
     "If no argument is given, formats standard in. Otherwise formats the first argument as a file."
     "Always outputs to standard out."
     #:once-each
     [("-w" "--width") width "Format for width <width> (default: 120)"
      (current-page-width width)]
     #:handlers
     (case-lambda
       [(_acc) (current-input-port)]
       [(_acc file) (open-input-file file)])
     '("file")))

  (define write-lang-line #f)
  (cond
    [(regexp-match-peek #rx"#lang" ip) (void (read-line ip 'any))
                                       (set! write-lang-line #t)])

  (~> (ip)
      (parse-bestiary "stdin" _ #:syntax? #f)
      (pretty-bestiary #:lang-line? write-lang-line)
      (pretty-print #:page-width 120)))
