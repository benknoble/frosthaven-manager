#lang racket

; vim: lw+=define-bidi-match-expander,define-bidi-match-expander/coercions,page,define/page,lambda/page,define/summon,define/monster

(provide
  (contract-out
    [launch-server (-> state?
                       procedure?
                       (values string? (-> any)))]))

(require
  (prefix-in files: web-server/dispatchers/dispatch-files)
  (prefix-in filter: web-server/dispatchers/dispatch-filter)
  (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
  alexis/multicast
  file/convertible
  frosthaven-manager/constants
  frosthaven-manager/defns
  (prefix-in elements: frosthaven-manager/elements)
  frosthaven-manager/manager
  frosthaven-manager/observable-operator
  (submod frosthaven-manager/gui/rich-text-display model)
  json
  nat-traversal
  net/url
  (prefix-in pict: pict)
  (except-in racket/gui
             null
             newline
             #%app)
  racket/gui/easy
  racket/runtime-path
  racket/async-channel
  syntax/parse/define
  (prefix-in tx: txexpr)
  web-server/dispatch/syntax
  web-server/dispatch/url-patterns
  web-server/dispatch/extend
  web-server/dispatchers/filesystem-map
  (prefix-in form: web-server/formlets)
  web-server/http
  web-server/http/bindings
  web-server/managers/lru
  web-server/page
  web-server/servlet-dispatch
  web-server/servlet/web
  web-server/web-server
  (only-in xml xexpr->string string->xexpr))

;;;; PREAMBLE
;;;; Macros, parameters, other definitions

(define-runtime-path static "static")

;; simplify calling convention for server-generated pages
(define s (make-parameter #f))
(define reverse-uri (make-parameter #f))
(define send-event (make-parameter #f))

;; "do" e in a different context… (namely, the one which invoked the server)
(define-syntax-parser do
  [(_ e:expr ...+)
   (syntax/loc this-syntax
     ((send-event)
      (thunk e ...)))])

(define-syntax-rule (define-bidi-match-expander/coercions id in-test? in out-test? out)
  (begin
    (define-coercion-match-expander in/m in-test? in)
    (define-coercion-match-expander out/m out-test? out)
    (define-bidi-match-expander id in/m out/m)))

(define-constant-format/parse
 format-element parse-element
 ([elements:fire "Fire"]
  [elements:ice "Ice"]
  [elements:air "Air"]
  [elements:earth "Earth"]
  [elements:light "Light"]
  [elements:dark "Dark"]))

(define-constant-format/parse
 format-element-style parse-element-style
 ([elements:element-pics-infused "infused"]
  [elements:element-pics-waning "waning"]
  [elements:element-pics-unfused "unfused"]))

(define-bidi-match-expander/coercions element-name-arg
  (or/c "Fire" "Ice" "Air" "Earth" "Light" "Dark") parse-element
  {(one-of? elements:fire elements:ice elements:air
            elements:earth elements:light elements:dark)} format-element)

(define-bidi-match-expander/coercions element-style-arg
  (or/c "infused" "waning" "unfused") string->symbol
  (or/c 'infused 'waning 'unfused) symbol->string)

;; evaluates e if player id (pid) and summon id (sid) can be extracted from req
(define-syntax-parser define/summon
  [(_ name:id (req:id {~literal =>} [pid:id sid:id]) e:expr ...+)
   (syntax/loc this-syntax
     (define (name req)
       (-do-summon req (λ (req pid sid) e ...))))])

;; evalutes e if monster group id and monster number can be extracted from req
(define-syntax-parser define/monster
  [(_ name:id (req:id {~literal =>} [monster-group-id:id monster-number:id])
      e:expr ...+)
   (syntax/loc this-syntax
     (define (name req)
       (-do-monster req (λ (req monster-group-id monster-number) e ...))))])

;; safe way to evaluate selector:condition, which contract errors when the
;; input is outside the domain; if this produces #t, input is a valid condition
;; number
(define selector:condition? (make-coerce-safe? selector:condition))

;; formlet shorthand
(define input-int
  (let ([numeric-text-input (form:text-input #:attributes '([inputmode "numeric"]))])
    (form:to-number (form:to-string (form:required numeric-text-input)))))

;;;; MAIN ENTRYPOINT

(define (launch-server an-s a-send-event)
  ;; each request thread get its own receiver, so that they can all see the
  ;; updates
  (define ch (make-multicast-channel))
  (for ([e (elements:elements)]
        [@e-state (state-@elements an-s)])
    (obs-observe!
      @e-state
      (λ (new-e-state)
        (multicast-channel-put ch `(element ,(elements:element-pics-name e) ,new-e-state)))))
  (obs-observe!
   (state-@creatures an-s)
   (let ([ids (box null)])
     (λ (creatures)
       (define env (@! (state-@env an-s)))
       (define ads (@! (state-@ability-decks an-s)))
       (for ([c creatures])
         (cond
           [(player? (creature-v c)) (multicast-channel-put ch `(player ,c))]
           [(creature-is-mg*? c) (multicast-channel-put ch `(monster-group* ,c ,env ,ads))]))
       (define new-ids (map creature-css-id (sort creatures < #:key (creature-initiative ads))))
       (unless (equal? (unbox ids) new-ids)
         (set-box! ids new-ids)
         (multicast-channel-put ch `(reorder ,(unbox ids)))))))
  (obs-observe! (state-@round an-s) (λ (r) (multicast-channel-put ch `(number round ,r))))
  (obs-observe! (state-@num-players an-s)
                (λ (n) (multicast-channel-put ch `(number inspiration ,(inspiration-reward n)))))
  (obs-observe!
    (state-@level an-s)
    (λ (level)
      (for ([id '(trap hazardous-terrain gold xp)]
            [f (list level-info-trap-damage level-info-hazardous-terrain level-info-gold level-info-exp)])
        (define n (~> (level) get-level-info f))
        (multicast-channel-put ch `(number ,id ,n)))))
  (obs-observe!
    (state-@in-draw? an-s)
    (λ (in-draw?)
      (define env (@! (state-@env an-s)))
      (define ads (@! (state-@ability-decks an-s)))
      (define cs (@! (state-@creatures an-s)))
      (for ([c cs])
        (cond
          [(player? (creature-v c)) (multicast-channel-put ch `(player ,c))]
          [(creature-is-mg*? c) (multicast-channel-put ch `(monster-group* ,c ,env ,ads))]))
      (define ids (map creature-css-id (sort cs < #:key (creature-initiative ads))))
      (multicast-channel-put ch `(reorder ,ids))
      (multicast-channel-put ch `(text progress-game ,(if in-draw? "Next Round" "Draw Abilities")))))
  (obs-observe!
   (state-@env an-s)
   (λ (env)
     (define ads (@! (state-@ability-decks an-s)))
     (for ([c (@! (state-@creatures an-s))])
       (cond
         [(creature-is-mg*? c) (multicast-channel-put ch `(monster-group* ,c ,env ,ads))]))))
  (obs-observe!
   (state-@ability-decks an-s)
   (λ (ads)
     (define env (@! (state-@env an-s)))
     (for ([c (@! (state-@creatures an-s))])
       (cond
         [(creature-is-mg*? c) (multicast-channel-put ch `(monster-group* ,c ,env ,ads))]))))
  (obs-observe!
   (state-@modifier an-s)
   (λ (m)
     (multicast-channel-put ch `(text modifier-discard ,(~a (if m
                                                                (format-monster-modifier m)
                                                                "N/A"))))))

  (define-values (app the-reverse-uri)
    (dispatch-rules
      [("") overview]
      [("rewards") rewards]
      [("discard-pile") discard-pile]
      [("action" "player" (string-arg) (string-arg) ...) #:method "post" player-action]
      [("action" "summon" (string-arg) (string-arg) ...) #:method "post" summon-action]
      [("action" "monster" (string-arg) (string-arg) ...) #:method "post" monster-action]
      [("action" "element" "transition") #:method "post" element-transition]
      [("action" "draw-modifier") #:method "post" web-draw-modifier]
      [("action" "progress-game") #:method "post" progress-game]
      [("events") (event-source ch)]
      [("element-pics" (element-name-arg) (element-style-arg)) element-pic]
      [else not-found]))

  (define url->path/static
    (make-url->path static))

  (define static-dispatcher
    (files:make #:url->path (λ (u)
                              (url->path/static
                                (struct-copy url u [path (cdr (url-path u))])))
                #:cache-no-cache #t))

  (define port-ch (make-async-channel 1))
  (define manager
    (make-threshold-LRU-manager expired-page (* 1024 1024 1024)))
  (define stop
    (parameterize ([s an-s]
                   [reverse-uri the-reverse-uri]
                   [send-event a-send-event])
      (serve
       #:dispatch (sequencer:make
                   (filter:make #rx"^/static/" static-dispatcher)
                   (dispatch/servlet app #:manager manager))
       #:port (if (file-exists? ".frosthaven-manager-port")
                (file->value ".frosthaven-manager-port")
                0)
       #:confirmation-channel port-ch)))
  (match (async-channel-get port-ch)
    [(? port-number? port) (values (~a "http://" (best-interface-ip-address) ":" port) stop)]
    [(? exn? e) (raise e)]))

;;;; X-EXPRS

(define common-heads
  `((meta ([name "viewport"] [content "width=device-width, initial-scale=1.0"]))
    (meta ([charset "UTF-8"]))
    (link ([rel "stylesheet"] [href "/static/style.css"]))))

(define (expired-page req)
  (response/xexpr
    `(html
       (head
         (title "Page Has Expired.")
         ,@common-heads)
       (body
         (p "Sorry, this page has expired. "
            (a ([href ,(url->string (url-sans-param (request-uri req)))])
               "This page")
            " may be the one you wanted.")))))

(define (not-found _req)
  (response/xexpr
    #:code 404
    `(html
       (head
         (title "Not Found")
         ,@common-heads)
       (body
         (h1 "Not Found")))))

(define/page (overview)
  (response/xexpr
    `(html
       (head
         (title "Frosthaven Manager")
         (script ([src "/static/events.js"]))
         ,@common-heads)
       (body
         (h1 "Frosthaven Manager")
         ,@(elements-body embed/url)
         ,@(creatures-body embed/url)
         ,@(bottom-info-body embed/url)
         ))))

(define/page (rewards)
  (define players
    (for/list ([c (@! (state-@creatures (s)))]
               #:when (player? (creature-v c)))
      (creature-v c)))
  (define num-players (@! (state-@num-players (s))))
  (define level (@! (state-@level (s))))
  (define level-info (get-level-info level))
  (define gold-factor (level-info-gold level-info))
  (define bonus-xp (level-info-exp level-info))
  (response/xexpr
   `(html
     (head
      (title "Frosthaven Manager Rewards")
      ,@common-heads)
     (body
      (h1 "Rewards")
      (div
       ([class "table-wrapper"])
       (table
        (thead
         (tr (th "Player")
             (th "Random Item?")
             (th "XP")
             (th "Gold")
             ,@(map {~>> format-material-kind (list 'th)} material-kinds)
             ,@(map {~>> format-herb-kind (list 'th)} herb-kinds)
             (th "Special Loot")))
        (tbody
         ,@(for/list ([p players])
             (define loots (player->rewards p num-players level))
             `(tr ,@(map {~>> (list 'td)} loots))))))
      (p "Gold Conversion Rate: " ,(~a gold-factor))
      (p "Bonus Experience: " ,(~a bonus-xp))
      ;; individual loot cards
      ,@(append*
         (for/list ([p players])
           `((h2 ,(~a (player-name p) "'s Loot Cards"))
             (ul
              ,@(for/list ([loot-text (map (format-loot-card num-players) (player-loot p))])
                  `(li ,loot-text))))))))))

(define/page (discard-pile)
  (define discard (@! (state-@monster-discard (s))))
  (response/xexpr
   `(html
     (head
      (title "Frosthaven Manager Discard Pile")
      ,@common-heads)
     (body
      (h1 "Discard Pile")
      (p "Most Recent First")
      (ol
       ,@(for/list ([m discard])
           `(li ,(format-monster-modifier m))))))))

(define (bottom-info-body embed/url)
  (define level-info (@! (@> (state-@level (s)) get-level-info)))
  (define num-players (@! (state-@num-players (s))))
  (define in-draw? (@! (state-@in-draw? (s))))
  (define discard (@! (@> (state-@modifier (s)) {(if _ format-monster-modifier "N/A")})))
  `((div
     ([class "bottom-info"])
     (p (button
         ([type "button"]
          [onclick
           ,(string-join
             (list
              (string-trim (action-script (list "draw-modifier") empty)
                           ";"
                           #:left? #f)
              ".then((r) => r.json())"
              ".then((j) => { alert(`The monster drew: ${j.modifier}.`); },"
              "      (_) => { /* silence the error: empty response */ })"))])
         "Draw Modifier")
        (span ([id "modifier-discard"])
              ,discard)
        " "
        (a ([href "/discard-pile"]) "Discard Pile")
        ,(action-button
          (list "progress-game")
          empty
          (if in-draw?
              "Next Round"
              "Draw Abilities")
          '([id "progress-game"])))
     (p "Round "
        (span ([id "round"])
              ,(number->string (@! (state-@round (s)))))
        ". "
        "Trap: "
        (span ([id "trap"])
              ,(number->string (level-info-trap-damage level-info)))
        ". "
        "Hazardous Terrain: "
        (span ([id "hazardous-terrain"])
              ,(number->string (level-info-hazardous-terrain level-info)))
        ". "
        "Gold: "
        (span ([id "gold"])
              ,(number->string (level-info-gold level-info)))
        ". "
        "Bonus XP: "
        (span ([id "xp"])
              ,(number->string (level-info-exp level-info)))
        ". "
        "Inspiration: "
        (span ([id "inspiration"])
              ,(number->string (inspiration-reward num-players)))
        ". "
        (a ([href "/rewards"]) "Rewards.")))))

(define (elements-body embed/url)
  `((h2 "Elements")
    (div ([id "elements"])
         ,@(for/list ([e (list elements:fire elements:ice elements:air elements:earth elements:light elements:dark)]
                      [@e-state (state-@elements (s))])
             `(img ([id ,(format-element e)]
                    [src ,((reverse-uri) element-pic e (@! @e-state))]
                    ,(action-click
                      (list "element" "transition")
                      (list (list (~s "id") (~s (format-element e)))))))))))

(define (creatures-body embed/url)
  `((h2 "Creatures")
    (ul ([class "creatures"])
        ,@(let ([env (@! (state-@env (s)))]
                [ability-decks (@! (state-@ability-decks (s)))]
                [cs (@! (state-@creatures (s)))])
            (creatures-xexprs embed/url cs env ability-decks)))))

(define (creatures-xexprs embed/url cs env ability-decks)
  (for/list ([c (sort cs < #:key (creature-initiative ability-decks))])
    (define v (creature-v c))
    (cond
      [(player? v) (player-xexpr embed/url (creature-id c) v)]
      [(monster-group*? v)
       (define mg (monster-group*-mg v))
       (define ability (~>> (mg) monster-group-set-name (hash-ref ability-decks) ability-decks-current))
       (monster-group-xexpr (creature-id c) mg ability env)])))

(define (player-xexpr embed/url id p)
  (define id-binding (list (~s "id") (~s (~s id))))
  `(li ([id ,(player-css-id id)])
       (div ([class "smash-inline"])
            (span ([class "player-name"])
                  ,(player-name p))
            " ("
            (span ([class "player-initiative"])
                  ,(~a (player-initiative p)))
            ") "
            (a ([href ,(embed/url {(set-initiative-form id)})])
               "Set Initiative")
            (p ,(action-button
                 (list "player" "hp" "-")
                 (list id-binding)
                 "-")
               (span ([class "player-HP"])
                     ,(player->hp-text p))
               ,(action-button
                 (list "player" "hp" "+")
                 (list id-binding)
                 "+")
               ,(action-button
                 (list "player" "xp" "-")
                 (list id-binding)
                 "-")
               "XP: "
               (span ([class "player-XP"])
                     ,(~a (player-xp p)))
               ,(action-button
                 (list "player" "xp" "+")
                 (list id-binding)
                 "+")
               (button ([type "button"]
                        [onclick
                         ,(string-join
                           (list
                            (string-trim (action-script (list "player" "loot") (list id-binding)) ";" #:left? #f)
                            ".then((r) => r.json())"
                            ".then((j) => { alert(`You got ${j.loot}!`); },"
                            "      (_) => { alert('The loot deck is empty.'); })"))])
                       "Loot!")))
       (div ([class "smash-inline"])
            (p (select ([id ,(~a "select-conditions-" id)]
                        [aria-label ,(~a "Add conditions to " (player-name p))])
                       ,@(for/list ([c conditions])
                           `(option ([value ,(~a (discriminator:condition c))])
                                    ,(format-condition c))))
               ,(action-button
                 (list "player" "condition" "add")
                 (list id-binding
                       (list (~s "condition")
                             (~a "document.querySelector("
                                 (~s (~a "#select-conditions-" id))
                                 ").value")))
                 "Add Condition"))
            (p (span
                ([class "player-conditions"])
                (span
                 ,@(~> (p) player-conditions* (active-conditions->xexpr id-binding))))))
       (p (a ([href ,(embed/url {(new-summon-form id)})])
             "Summon"))
       (ol ([class "summons"])
           ,@(summons->xexprs id (player-summons p)))))

(define set-initiative
  (form:formlet
   (form:#%#
    (p "Initiative" ,{=> input-int init})
    (p ,{=> (form:submit "Set Initiative") _submit}))
   init))

(define/page (set-initiative-form player-id)
  (define (handle-form-response r)
    (define form-response
      (with-handlers ([exn:fail? values])
        (form:formlet-process set-initiative r)))
    (match form-response
      [(? initiative? init) (set-player-initiative player-id init)]
      [_ (void)])
    (my-redirect/get ((reverse-uri) overview)))
  (response/xexpr
   `(html
     (head (title "Set Initiative") ,@common-heads)
     (body
      (form ([action ,(embed/url handle-form-response)]
             [method "post"])
            ,@(form:formlet-display set-initiative))))))

;; s: summon?
;; id: string? unique to whole page
(define (summon-xexpr s id)
  (define id-binding (list (~s "id") (~s id)))
  `(li ([id ,id])
       ,(action-button
         (list "summon" "kill")
         (list id-binding)
         "Kill")
       (span ([class "summon-name"])
             ,(summon-name s))
       ,(action-button
           (list "summon" "hp" "-")
           (list id-binding)
           "-")
       (span ([class "summon-HP"])
             ,(summon->hp-text s))
       ,(action-button
         (list "summon" "hp" "+")
         (list id-binding)
         "+")
       (div ([class "smash-inline"])
            (p (select ([id ,(~a "select-conditions-" id)]
                        [aria-label ,(~a "Add conditions to " (summon-name s))])
                       ,@(for/list ([c conditions])
                           `(option ([value ,(~a (discriminator:condition c))])
                                    ,(format-condition c))))
               ,(action-button
                 (list "summon" "condition" "add")
                 (list id-binding
                       (list (~s "condition")
                             (~a "document.querySelector("
                                 (~s (~a "#select-conditions-" id))
                                 ").value")))
                 "Add Condition"))
            (p (span
                ([class "summon-conditions"])
                (span
                 ,@(~> (s) summon-conditions* (active-conditions->xexpr id-binding "summon"))))))))

;; required => exn:fail if not present
(define new-summon
  (form:formlet
   (form:#%#
    (p "Name:" ,{=> form:input-string name})
    (p "Max HP:" ,{=> input-int max-hp})
    (p ,{=> (form:submit "Summon") _submit}))
   (list name max-hp)))

(define/page (new-summon-form player-id)
  (define (handle-form-response r)
    (define form-response
      (with-handlers ([exn:fail? values])
        (form:formlet-process new-summon r)))
    (match form-response
      [(list name max-hp)
       (do-player/id player-id
                     (const #f)
                     {(player-summon name max-hp)})]
      [_ (void)])
    (my-redirect/get ((reverse-uri) overview)))
  (response/xexpr
   `(html
     (head (title "Summon") ,@common-heads)
     (body
      (form ([action ,(embed/url handle-form-response)]
             [method "post"])
            ,@(form:formlet-display new-summon))))))

(define (summons->xexprs summoner-id ss)
  (for/list ([(s i) (in-indexed ss)])
    (define id (~a "summon-" summoner-id "-" i))
    (summon-xexpr s id)))

(define (monster-group-xexpr id mg ability env)
  (define normal-stats (monster-group-normal-stats mg))
  (define elite-stats (monster-group-elite-stats mg))
  (define (both-empty? f)
    (~> (normal-stats elite-stats)
        (>< f) (all empty?)))
  (define (empty-row label f sf)
    (if (both-empty? f)
      empty
      `((tr (td ,(sf normal-stats))
            (td ,label)
            (td ,(sf elite-stats))))))
  `(li ([id ,(monster-group-css-id id)])
       (span ([class "monster-group-name"])
             ,(monster-group-name mg))
       " ("
       (span ([class "monster-group-initiative"])
             ,(monster-ability-initiative->text ability))
       ")"
       (details
        ([class "stats-summary"])
        (summary "Stats")
        (table
         ([class "monster-group-stats"])
         (tr (th "Normal") (th "Stat") (th "Elite"))
         (tr (td ,(~> (normal-stats) monster-stats-move (if _ ~a "-")))
             (td "Move")
             (td ,(~> (elite-stats) monster-stats-move (if _ ~a "-"))))
         (tr (td ,(~a (monster-stats-attack* normal-stats env)))
             (td "Attack")
             (td ,(~a (monster-stats-attack* elite-stats env))))
         ,@(empty-row "Bonuses" monster-stats-bonuses monster-stats-bonuses-string)
         ,@(empty-row "Effects" monster-stats-effects monster-stats-effects-string)
         ,@(empty-row "Immunities" monster-stats-immunities monster-stats-immunities-string)
         (tr (td ,(~a (monster-stats-max-hp* normal-stats env)))
             (td "Max HP")
             (td ,(~a (monster-stats-max-hp* elite-stats env))))))
       (p ([class "monster-ability"])
          (span ([class "monster-ability-name"]) ,(monster-ability-name->text ability))
          (ol ([class "monster-ability-abilities"])
              ,@(monster-ability-xexpr mg ability env)))
       (ul ([class "monsters"])
           ,@(monsters->xexprs id (monster-group-monsters mg) mg env))))

(define (monster-ability-xexpr mg ability env)
  (for/list ([the-ability (if ability (monster-ability-abilities ability) empty)])
    `(li
      (span
       ([class "monster-ability-ability"])
       ,@(for/list ([content (in-list (monster-ability-ability->rich-text the-ability ability mg env))])
           (match content
             [(? string? s) s]
             [(? newline?) `(br)]
             [(or (? pict:pict? p) (pict/alt-text p _))
              (define svg (string->xexpr (bytes->string/utf-8 (convert p 'svg-bytes))))
              (cond
                [(>= (pict:pict-height p) 50) svg]
                [else (tx:attr-set svg 'class "icon")])]))))))

(define (monsters->xexprs group-id monsters mg env)
  (for/list ([monster monsters])
    (define id (~a "monster-" group-id "-" (monster-number monster)))
    (monster-xexpr id monster mg env)))

(define (monster-xexpr id m mg env)
  (define id-binding (list (~s "id") (~s id)))
  `(li ([id ,id])
        ,(action-button (list "monster" "kill")
                        (list id-binding)
                        "Kill")
        (span ([class "monster-number"])
              ,(~a (monster-group-name mg) " ")
              ,(~a (monster-number m))
              ,(if (monster-elite? m) "(E)" ""))
        ,(action-button (list "monster" "hp" "-")
                        (list id-binding)
                        "-")
        (span ([class "monster-HP"])
              ,(monster->hp-text m (get-monster-stats mg m) env))
        ,(action-button (list "monster" "hp" "+")
                        (list id-binding)
                        "+")
        (div ([class "smash-inline"])
             (p (select ([id ,(~a "select-conditions-" id)]
                         [aria-label ,(~a "Add conditions to " (monster-group-name mg) " " (monster-number m))])
                        ,@(for/list ([c conditions])
                            `(option ([value ,(~a (discriminator:condition c))])
                                     ,(format-condition c))))
                ,(action-button
                  (list "monster" "condition" "add")
                  (list id-binding
                        (list (~s "condition")
                              (~a "document.querySelector("
                                  (~s (~a "#select-conditions-" id))
                                  ").value")))
                  "Add Condition"))
             (p (span ([class "monster-conditions"])
                      (span
                       ,@(~> (m) monster-conditions (active-conditions->xexpr id-binding "monster"))))))))

(define (action-button actions bindings body [attrs empty])
  `(button ([type "button"]
            ,(action-click actions bindings)
            ,@attrs)
           ,body))

(define (active-conditions->xexpr cs id-binding [who "player"])
  (~> (cs)
      (map {(active-condition->xexpr id-binding who)} _)
      (add-between ", " #:before-last " and ")))

(define (active-condition->xexpr c id-binding [who "player"])
  `(span ([class "condition"])
         ,(format-condition c)
         ,(action-button
            (list who "condition" "remove")
            (list id-binding
                  (list (~s "condition") (~s (~s (discriminator:condition c)))))
            "X")))

;;;; ELEMENT PICTURES

(define (element-pic _req name style)
  (response/output
    (λ (o)
      (write-bytes (convert (get-pic name style) 'svg-bytes) o))
    #:mime-type #"image/svg+xml"))

(define (get-pic pics style)
  ((parse-element-style (symbol->string style))
   (pics)))

;;;; SSE

(define ((event-source ch) _req)
  (define receiver (make-multicast-receiver ch))
  (response/output
    #:headers (list (header #"Cache-Control" #"no-store")
                    (header #"Content-Type" #"text/event-stream")
                    ;; Don't use Connection in HTTP/2 or HTTP/3, but Racket's
                    ;; web-server is HTTP/1.1 as confirmed by
                    ;; `curl -vso /dev/null --http2 <addr>`.
                    (header #"Connection" #"keep-alive")
                    ;; Pairs with Connection; since our event source sends data
                    ;; every 5 seconds at minimum, this 10s timeout should be
                    ;; sufficient.
                    (header #"Keep-Alive" #"timeout=10"))
    (λ (out)
      (let loop ()
        (cond
          [(sync/timeout 5 receiver) => (event-stream out)]
          [else (displayln ":" out)])
        (loop)))))

(define (event-stream out)
  (match-lambda
    [`(element ,name ,state)
      (define data (hash 'name name 'state (symbol->string state)))
      (displayln "event: element" out)
      (display (format "data: ~a" (jsexpr->string data)) out)
      (display "\n\n" out)]
    [`(player ,c)
      (define p (creature-v c))
      (define id (creature-id c))
      (define id-binding (list (~s "id") (~s (~s id))))
      (define css-id (player-css-id id))
      (define data
        (hash 'id css-id
              'data (hash
                      'player-name (player-name p)
                      'player-initiative (~a (player-initiative p))
                      'player-HP (player->hp-text p)
                      'player-XP (~a (player-xp p))
                      'player-conditions
                      (~> (p) player-conditions*
                          (active-conditions->xexpr id-binding)
                          (cons 'span _)
                          xexpr->string))
              'summons (map xexpr->string (summons->xexprs id (player-summons p)))))
      (displayln "event: player" out)
      (display (format "data: ~a" (jsexpr->string data)) out)
      (displayln "\n\n" out)]
    [(list 'monster-group* c env ads)
     (define mg (monster-group*-mg (creature-v c)))
     (define id (creature-id c))
     (define css-id (monster-group-css-id id))
     (define ability (~>> (mg) monster-group-set-name (hash-ref ads) ability-decks-current))
     (define data
       (hash 'id css-id
             'data (hash
                    'monster-group-name (monster-group-name mg)
                    'monster-group-initiative (monster-ability-initiative->text ability)
                    'monster-ability-name (monster-ability-name->text ability)
                    'monster-ability-abilities (~>> (mg ability env)
                                                    monster-ability-xexpr
                                                    (map xexpr->string))
                    'monsters (~>> (id (monster-group-monsters mg) mg env)
                                   monsters->xexprs
                                   (map xexpr->string)))
             'xexpr (xexpr->string (monster-group-xexpr id mg ability env))))
     (displayln "event: monster-group" out)
     (display (format "data: ~a" (jsexpr->string data)) out)
     (displayln "\n\n" out)]
    [`(reorder ,ids)
     (define data ids)
     (displayln "event: reorder-ids" out)
     (display (format "data: ~a" (jsexpr->string data)) out)
     (displayln "\n\n" out)]
    [`(number ,id ,(? number? n))
      (define data (hash 'id (~a id) 'n n))
      (displayln "event: number" out)
      (display (format "data: ~a" (jsexpr->string data)) out)
      (displayln "\n\n" out)]
    [`(text ,id ,(? string? s))
     (define data (hash 'id (~a id) 'text s))
     (displayln "event: text" out)
     (display (format "data: ~a" (jsexpr->string data)) out)
     (displayln "\n\n" out)]))

;;;; ACTIONS

(define (player-action req what args)
  (let/ec return
    (match (cons what args)
      ['("hp" "+") (increment-player-hp req)]
      ['("hp" "-") (decrement-player-hp req)]
      ['("xp" "+") (increment-player-xp req)]
      ['("xp" "-") (decrement-player-xp req)]
      ['("condition" "remove") (remove-player-condition req)]
      ['("condition" "add") (add-player-condition req)]
      ['("loot") (loot! req return)]
      [_ (return (not-found req))])
    (response/empty)))

(define (summon-action req what args)
  (let/ec return
    (match (cons what args)
      ['("kill") (kill-summon req)]
      ['("hp" "-") (decrement-summon-hp req)]
      ['("hp" "+") (increment-summon-hp req)]
      ['("condition" "add") (add-summon-condition req)]
      ['("condition" "remove") (remove-summon-condition req)]
      [_ (return (not-found req))])
    (response/empty)))

(define (monster-action req what args)
  (let/ec return
    (match (cons what args)
      ['("kill") (kill-monster req)]
      ['("hp" "-") (decrement-monster-hp req)]
      ['("hp" "+") (increment-monster-hp req)]
      ['("condition" "add") (add-monster-condition req)]
      ['("condition" "remove") (remove-monster-condition req)]
      [_ (return (not-found req))])
    (response/empty)))

(define (element-transition req)
  (match (assq 'id (request-bindings req))
    [`(id . ,(element-name-arg e))
      (define @e-state
        (~>> ((list elements:fire elements:ice elements:air
                    elements:earth elements:light elements:dark)
              (state-@elements (s)))
             (map cons) (assq e) cdr))
      (do (<@ @e-state transition-element-state))
      (response/empty)]
    [(or `(id . ,_) #f) (not-found req)]))

(define (web-draw-modifier _req)
  (do ((draw-modifier (s))))
  (cond
    [(@! (state-@modifier (s))) => {~>> format-monster-modifier (hash 'modifier) response/jsexpr}]
    [else (response/empty)]))

(define (progress-game _req)
  (do
    (let ([s (s)])
      ;; double-parens: apply function that this if expression evaluates to
      ((if (@! (state-@in-draw? s))
           (next-round s)
           (draw-abilities s)))))
  (response/empty))

(define-flow (increment-player-hp _req)
  (do-player player-at-max-health? (player-act-on-hp add1)))

(define-flow (decrement-player-hp _req)
  (do-player player-dead? (player-act-on-hp sub1)))

(define-flow (increment-player-xp _req)
  (do-player (const #f) (player-act-on-xp add1)))

(define-flow (decrement-player-xp _req)
  (do-player {~> player-xp zero?} (player-act-on-xp sub1)))

(define-flow (remove-player-condition _req)
  (do-player-condition #f))

(define-flow (add-player-condition _req)
  (do-player-condition #t))

(define (set-player-initiative id init)
  (do (<@ (state-@creatures (s))
          {(update-players id {(player-set-initiative init)})})))

(define (loot! req return)
  (match (req->player-id req)
    [(and id (not #f))
     (define card
       (@! (@> (state-@loot-deck (s)) {(and (not empty?) first)})))
     (do ((give-player-loot (s)) id))
     (when card
       (return
        (response/jsexpr
         (hash 'loot ((format-loot-card (@! (state-@num-players (s)))) card)))))]
    [_ (void)]))

(define/summon kill-summon (_r => [pid sid])
  (do-player/id pid (const #f) (player-kill-summon sid)))

(define/summon decrement-summon-hp (_r => [pid sid])
  (do-player/id pid
                {~> player-summons (list-ref sid) summon-dead?}
                (update-player-summon sid (summon-act-on-hp sub1))))

(define/summon increment-summon-hp (_r => [pid sid])
  (do-player/id pid
                {~> player-summons (list-ref sid) summon-at-max-health?}
                (update-player-summon sid (summon-act-on-hp add1))))

(define/summon add-summon-condition (req => [pid sid])
  (match (req->condition req)
    [(and c (not #f)) (do-player/id pid (const #f) (update-player-summon sid (summon-add-condition c)))]
    [_ (void)]))

(define/summon remove-summon-condition (req => [pid sid])
  (match (req->condition req)
    [(and c (not #f)) (do-player/id pid (const #f) (update-player-summon sid (summon-remove-condition c)))]
    [_ (void)]))

(define/monster kill-monster (_r => [mgid mn])
  (do-monster-group/mgid mgid (monster-group-remove mn) {~> 2> monster-group-first-monster}))

(define/monster decrement-monster-hp (_r => [mgid mn])
  (do-monster-group/n mgid mn {switch [(not monster-dead?) (esc (monster-update-hp sub1))]}))

(define/monster increment-monster-hp (_r => [mgid mn])
  (define inc-hp (monster-update-hp add1))
  (define ((inc-hp/mg mg) m)
    (define stats (get-monster-stats mg m))
    (cond
      [(monster-at-max-health? m stats (@! (state-@env (s)))) m]
      [else (inc-hp m)]))
  (define (inc-hp/mg/n mg)
    (define do-it (monster-group-update-num mn (inc-hp/mg mg)))
    (do-it mg))
  (do-monster-group/mgid mgid inc-hp/mg/n))

(define/monster add-monster-condition (req => [mgid mn])
  (match (req->condition req)
    [(and c (not #f)) (do-monster-group/n mgid mn (monster-update-condition c #t))]
    [_ (void)]))

(define/monster remove-monster-condition (req => [mgid mn])
  (match (req->condition req)
    [(and c (not #f)) (do-monster-group/n mgid mn (monster-update-condition c #f))]
    [_ (void)]))

;;;; HELPERS

(define (action-script actions bindings)
  (define URL (string-join (cons "action" actions) "/" #:before-first "/"))
  (define params
    (string-join
      (for/list ([b bindings])
        (match-define (list key value) b)
        (format "[~a, ~a]" key value))
      ","))
  (format "fetch('~a', {method: 'POST', body: new URLSearchParams([~a])});"
          URL params))

(define (action-click actions bindings)
  `[onclick ,(action-script actions bindings)])

(define (my-redirect/get where)
  (send/suspend
   (λ (_)
     (redirect-to where see-other))))

(define (url-sans-param u)
  (struct-copy url u [path (map path/param-sans-param (url-path u))]))

(define (path/param-sans-param pp)
  (struct-copy path/param pp [param empty]))

(define (do-player req guard action)
  (match (req->player-id req)
    [(and id (not #f)) (do-player/id id guard action)]
    [_ (void)]))

(define (do-player/id id guard action)
  (do (<@ (state-@creatures (s))
          {(update-players id {switch [(not guard) action]})})))

(define (do-player-condition req add-or-remove)
  (match* {(req->player-id req) (req->condition req)}
    [{(and id (not #f)) (and c (not #f))}
      (define c? (list c add-or-remove))
      (do (<@ (state-@creatures (s))
              {(update-players id (player-condition-handler c?))}))]
    [{_ _} (void)]))

(define (req->player-id r)
  (match (assq 'id (request-bindings r))
    [`(id . ,(app string->number (? number? id))) id]
    [_ #f]))

(define (req->player-summon-id r)
  (match (assq 'id (request-bindings r))
    [(cons 'id (regexp #px"summon-(\\d+)-(\\d+)"
                       (list _
                             (app string->number (? number? player-id))
                             (app string->number (? number? summon-id)))))
     (values player-id summon-id)]
    [_ (values #f #f)]))

(define (req->condition r)
  (match (assq 'condition (request-bindings r))
    [(cons 'condition
           (app string->number (? selector:condition? (app selector:condition c))))
     c]
    [_ #f]))

(define (-do-summon req f)
  (match/values (req->player-summon-id req)
    [{(and pid (not #f)) (and sid (not #f))} (f req pid sid)]
    [{_ _} (void)]))

(define (req->monster-ids r)
  (match (assq 'id (request-bindings r))
    [(cons 'id (regexp #px"monster-(\\d+)-(\\d+)"
                       (list _
                             (app string->number (? number? monster-group-id))
                             (app string->number (? number? monster-number)))))
     (values monster-group-id monster-number)]
    [_ (values #f #f)]))

(define (do-monster-group/mgid mgid action [action-n {1>}])
  (do (<@ (state-@creatures (s))
          {(update-monster-groups mgid action action-n)})))

(define (do-monster-group/n mgid mn action)
  (do-monster-group/mgid mgid (monster-group-update-num mn action)))

(define (-do-monster req f)
  (match/values (req->monster-ids req)
    [{(and mgid (not #f)) (and mn (not #f))} (f req mgid mn)]
    [{_ _} (void)]))

(define-flow player-css-id (~a "player-" _))
(define-flow monster-group-css-id (~a "monster-group-" _))
(define-switch (creature-css-id _c)
  (% creature-v creature-id)
  [player? player-css-id]
  [monster-group*? monster-group-css-id])
