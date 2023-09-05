#lang racket

(require racket/gui/easy
         frosthaven-manager/observable-operator
         (only-in frosthaven-manager/elements
                  elements
                  element-pics-name)
         (submod frosthaven-manager/manager/state unsafe))

(define (~a-view lab f s)
  (input #:label lab (@> (f s) ~a) #:enabled? #f))

(define (pp-view lab f s)
  (input #:label lab (@> (f s) pretty-format) #:style '(multiple) #:enabled? #f
         #:min-size '(0 100)))

(define (plain-view s)
  (vpanel
   #:style '(vscroll)
   (~a-view "mode" state-@mode s)
   (~a-view "level" state-@level s)
   (~a-view "num-players" state-@num-players s)
   (pp-view "creatures" state-@creatures s)
   (pp-view "cards-per-deck" state-@cards-per-deck s)
   (pp-view "loot-deck" state-@loot-deck s)
   (~a-view "num-loot-cards" state-@num-loot-cards s)
   (apply hpanel
          #:stretch '(#t #f)
          (for/list ([@es (state-@elements s)]
                     [e (elements)])
            (input #:label (element-pics-name e) (@> @es ~a) #:enabled? #f)))
   (~a-view "in-draw?" state-@in-draw? s)
   (~a-view "round" state-@round s)
   (pp-view "monster-modifier-deck" state-@monster-modifier-deck s)
   (pp-view "monster-discard" state-@monster-discard s)
   (pp-view "player-blesses" state-@player-blesses s)
   (pp-view "curses" state-@curses s)
   (pp-view "blesses" state-@blesses s)
   (pp-view "modifier" state-@modifier s)
   (pp-view "monster-prev-discard" state-@monster-prev-discard s)
   (pp-view "info-db" state-@info-db s)
   (pp-view "ability-db" state-@ability-db s)
   (pp-view "ability-decks" state-@ability-decks s)
   (pp-view "stickers-per-loot-deck" state-@stickers-per-loot-deck s)
   (apply hpanel
          #:stretch '(#t #f)
          (for/list ([key '("L" "C")])
            (input #:label key (@~> (state-@env s) (~> (hash-ref key) ~a)) #:enabled? #f)))))

(module+ main
  (command-line
   #:args (file)
   (define s (call-with-input-file file deserialize-state))
   (render (window #:size '(300 400) (plain-view s)))))
