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
  frosthaven-manager/defns
  (prefix-in elements: frosthaven-manager/elements)
  frosthaven-manager/gui/elements
  frosthaven-manager/manager
  frosthaven-manager/observable-operator
  frosthaven-manager/qi
  json
  nat-traversal
  net/url
  (except-in racket/gui null)
  racket/gui/easy
  racket/runtime-path
  syntax/parse/define
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
(define-syntax-parse-rule (do e:expr ...+)
  ((send-event)
   (thunk e ...)))

(define-syntax-rule (define-bidi-match-expander/coercions id in-test? in out-test? out)
  (begin
    (define-coercion-match-expander in/m in-test? in)
    (define-coercion-match-expander out/m out-test? out)
    (define-bidi-match-expander id in/m out/m)))

(define-bidi-match-expander/coercions element-name-arg
  (or/c "Fire" "Ice" "Air" "Earth" "Light" "Dark") string->symbol
  (or/c 'Fire 'Ice 'Air 'Earth 'Light 'Dark) symbol->string)

(define-bidi-match-expander/coercions element-style-arg
  (or/c "infused" "waning" "unfused") string->symbol
  (or/c 'infused 'waning 'unfused) symbol->string)

;; evaluates e if player id (pid) and summon id (sid) can be extracted from req
(define-syntax-parse-rule (define/summon name:id (req:id {~literal =>} [pid:id sid:id]) e:expr ...+)
  (define (name req)
    (-do-summon req (λ (req pid sid) e ...))))

;; evalutes e if monster group id and monster number can be extracted from req
(define-syntax-parse-rule (define/monster name:id (req:id {~literal =>} [monster-group-id:id monster-number:id])
                                          e:expr ...+)
  (define (name req)
    (-do-monster req (λ (req monster-group-id monster-number) e ...))))

;; safe way to evaluate selector:condition, which contract errors when the
;; input is outside the domain; if this produces #t, input is a valid condition
;; number
(define selector:condition? (make-coerce-safe? selector:condition))

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
    (λ (creatures)
      (define env (@! (state-@env an-s)))
      (define ads (@! (state-@ability-decks an-s)))
      (for ([c creatures])
        (cond
          [(player? (creature-v c)) (multicast-channel-put ch `(player ,c))]
          [(monster-group*? (creature-v c)) (multicast-channel-put ch `(monster-group* ,c ,env ,ads))]))))
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
    (λ (_in-draw?)
      (define env (@! (state-@env an-s)))
      (define ads (@! (state-@ability-decks an-s)))
      (define cs (@! (state-@creatures an-s)))
      (for ([c cs])
        (cond
          [(player? (creature-v c)) (multicast-channel-put ch `(player ,c))]
          [(monster-group*? (creature-v c)) (multicast-channel-put ch `(monster-group* ,c ,env ,ads))]))
      (define ids (map creature-css-id (sort cs < #:key (creature-initiative ads))))
      (multicast-channel-put ch `(reorder ,ids))))
  (obs-observe!
   (state-@env an-s)
   (λ (env)
     (define ads (@! (state-@ability-decks an-s)))
     (for ([c (@! (state-@creatures an-s))])
       (cond
         [(monster-group*? (creature-v c)) (multicast-channel-put ch `(monster-group* ,c ,env ,ads))]))))
  (obs-observe!
   (state-@ability-decks an-s)
   (λ (ads)
     (define env (@! (state-@env an-s)))
     (for ([c (@! (state-@creatures an-s))])
       (cond
         [(monster-group*? (creature-v c)) (multicast-channel-put ch `(monster-group* ,c ,env ,ads))]))))

  (define-values (app the-reverse-uri)
    (dispatch-rules
      [("") overview]
      [("rewards") rewards]
      [("action" "player" (string-arg) (string-arg) ...) #:method "post" player-action]
      [("action" "summon" (string-arg) (string-arg) ...) #:method "post" summon-action]
      [("action" "monster" (string-arg) (string-arg) ...) #:method "post" monster-action]
      [("action" "element" "transition") #:method "post" element-transition]
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

  (define port 8000)
  (define manager (make-threshold-LRU-manager expired-page (* 64 1024 1024)))
  (values
    (~a "http://" (best-interface-ip-address) ":" port)
    (parameterize ([s an-s]
                   [reverse-uri the-reverse-uri]
                   [send-event a-send-event])
      (serve
        #:dispatch (sequencer:make
                     (filter:make #rx"^/static/" static-dispatcher)
                     (dispatch/servlet app #:manager manager))
        #:port port))))

;;;; X-EXPRS

(define common-heads
  `((meta ([name "viewport"] [content "width=device-width, initial-scale=1.0"]))
    (meta ([charset "UTF-8"]))))

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
         ,@(top-info-body embed/url)
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
  (define gold-factor (level-info-gold (get-level-info level)))
  (define-flow get-material-amount (~> material-amount (list-ref (- num-players 2))))
  (define (find-materials m)
    (flow (and material? (~> material-name (equal? m)))))
  (define (find-herbs h)
    (flow (and herb? (~> herb-name (equal? h)))))
  (response/xexpr
   `(html
     (head
      (title "Frosthaven Manager Rewards")
      ,@common-heads
      (style #<<STYLE
table {
  width: 100%;
  border: 1px solid;
  border-collapse: collapse;
}
th, td {
  border: 1px solid;
  padding: 1%;
}
tbody tr:hover {
  background-color: powderblue;
}
.table-wrapper {
  width: 100%;
  overflow-x: auto;
}
STYLE
             ))
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
             ,@(map (flow (~>> ~a (list 'th))) material-kinds)
             ,@(map (flow (~>> ~a (list 'th))) herb-kinds)))
        (tbody
         ,@(for/list ([p players])
             (define loots (player-loot p))
             `(tr (td ,(player-name p))
                  (td ,(if (memf random-item? loots) "x" ""))
                  (td ,(~a (player-xp p)))
                  (td ,(~a (for/sum ([loot loots] #:when (money? loot))
                             (* (money-amount loot) gold-factor))))
                  ,@(for/list ([m material-kinds])
                      `(td ,(~a (for/sum ([loot (filter (find-materials m) loots)])
                                  (get-material-amount loot)))))
                  ,@(for/list ([h herb-kinds])
                      `(td ,(~a (for/sum ([loot (filter (find-herbs h) loots)])
                                  (herb-amount loot))))))))))
      (p "Gold Conversion Rate: " ,(~a gold-factor))
      ;; individual loot cards
      ,@(append*
         (for/list ([p players])
           `((h2 ,(~a (player-name p) "'s Loot Cards"))
             (ul
              ,@(for/list ([loot-text (map (format-loot-card num-players) (player-loot p))])
                  `(li ,loot-text))))))))))

(define (top-info-body embed/url)
  `((p "Round "
       (span ([id "round"])
             ,(number->string (@! (state-@round (s))))))))

(define (bottom-info-body embed/url)
  (define level-info (@! (@> (state-@level (s)) get-level-info)))
  (define num-players (@! (state-@num-players (s))))
  `((p "Trap: "
       (span ([id "trap"])
             ,(number->string (level-info-trap-damage level-info)))
       (br)
       "Hazardous Terrain: "
       (span ([id "hazardous-terrain"])
             ,(number->string (level-info-hazardous-terrain level-info)))
       (br)
       "Gold: "
       (span ([id "gold"])
             ,(number->string (level-info-gold level-info)))
       (br)
       "Bonus XP: "
       (span ([id "xp"])
             ,(number->string (level-info-exp level-info)))
       (br)
       "Inspiration: "
       (span ([id "inspiration"])
             ,(number->string (inspiration-reward num-players))))
    (a ([href "/rewards"]) "Rewards")))

(define (elements-body embed/url)
  `((h2 "Elements")
    ,@(for/list ([e (list 'Fire 'Ice 'Air 'Earth 'Light 'Dark)]
                 [@e-state (state-@elements (s))])
        `(img ([id ,(symbol->string e)]
               [src ,((reverse-uri) element-pic e (@! @e-state))]
               ,(action-click
                  (list "element" "transition")
                  (list (list (~s "id") (~s (symbol->string e))))))))))

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
       (span ([class "player-name"])
             ,(player-name p))
       " ("
       (span ([class "player-initiative"])
             ,(~a (player-initiative p)))
       ")"
       (input ([class "player-initiative-input"]
               [type "text"]
               [inputmode "numeric"]
               [value ,(~a (player-initiative p))]
               ;; TODO
               ;; - probably requires restructure: got to "pick" a
               ;; player
               ;; - update input values when player init changes, when
               ;; init revealed
               ;; - when init dragged, reveal ONLY current init (so that dragging works)
               [oninput
                ,(~a "for (element of document.querySelectorAll(\".player-initiative-input\")) { if (element !== this) { element.disabled = true; } }"
                     (action-script (list "player" "initiative")
                                    (list id-binding
                                          (list (~s "initiative") "this.value"))))]))
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
          ,(action-button
            (list "player" "loot")
            (list id-binding)
            "Loot!"))
       (p (select ([id ,(~a "select-conditions-" id)])
                  ,@(for/list ([c conditions])
                      `(option ([value ,(~a (discriminator:condition c))])
                               ,(~a c))))
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
            ,@(~> (p) player-conditions* (active-conditions->xexpr id-binding)))))
       (p (a ([href ,(embed/url (flow (new-summon-form id)))])
             "Summon"))
       (ol ([class "summons"])
           ,@(summons->xexprs id (player-summons p)))))

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
       (p (select ([id ,(~a "select-conditions-" id)])
                  ,@(for/list ([c conditions])
                      `(option ([value ,(~a (discriminator:condition c))])
                               ,(~a c))))
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
           ,@(~> (s) summon-conditions* (active-conditions->xexpr id-binding "summon")))))))

;; required => exn:fail if not present
(define new-summon
  (form:formlet
   (form:#%#
    "Name:" ,{=> form:input-string name}
    "Max HP:" ,{=> form:input-int max-hp}
    ,{=> (form:submit "Summon") _submit})
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
                     (flow (player-summon name max-hp)))]
      [_ (void)])
    (overview (redirect/get)))
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
  `(li ([id ,(monster-group-css-id id)])
       (span ([class "monster-group-name"])
             ,(monster-group-name mg))
       " ("
       (span ([class "monster-group-initiative"])
             ,(monster-ability-initiative->text ability))
       ")"
       (details
        ([style "display: inline; margin-left: 1%;"]
         [ontoggle "this.style = this.open ? \"display: block;\" : \"display: inline; margin-left: 1%;\";"])
        (summary "Stats")
        (table
         ([class "monster-group-stats"])
         (tr (th "Normal") (th "Stat") (th "Elite"))
         (tr (td ,(~a (monster-stats-move normal-stats)))
             (td "Move")
             (td ,(~a (monster-stats-move elite-stats))))
         (tr (td ,(~a (monster-stats-attack* normal-stats env)))
             (td "Attack")
             (td ,(~a (monster-stats-attack* elite-stats env))))
         ,@(if (both-empty? monster-stats-bonuses)
             empty
             `((tr (td ,(string-join (monster-stats-bonuses normal-stats) ", "))
                   (td "Bonuses")
                   (td ,(string-join (monster-stats-bonuses elite-stats) ", ")))))
         ,@(if (both-empty? monster-stats-effects)
             empty
             `((tr (td ,(string-join (monster-stats-effects normal-stats) ", "))
                   (td "Effects")
                   (td ,(string-join (monster-stats-effects elite-stats) ", ")))))
         ,@(if (both-empty? monster-stats-immunities)
             empty
             `((tr (td ,(string-join (monster-stats-immunities normal-stats) ", "))
                   (td "Immunities")
                   (td ,(string-join (monster-stats-immunities elite-stats) ", ")))))
         (tr (td ,(~a (monster-stats-max-hp* normal-stats env)))
             (td "Max HP")
             (td ,(~a (monster-stats-max-hp* elite-stats env))))))
       (p ([class "monster-ability"])
          (span ([class "monster-ability-name"]) ,(monster-ability-name->text ability))
          (ol ([class "monster-ability-abilities"]
               [style "list-style-type: none;"])
              ,@(monster-ability-xexpr mg ability env)))
       (div ([class "monsters"])
            ,@(monsters->xexprs id (monster-group-monsters mg) mg env))))

(define (monster-ability-xexpr mg ability env)
  (append*
   (for/list ([the-ability (if ability (monster-ability-abilities ability) empty)])
     (define extras (monster-ability-ability->extras ability the-ability))
     (cons
      `(li (span ([class "monster-ability-ability"])
                 ,((monster-ability-ability->text the-ability) mg env)))
      (for/list ([extra extras])
        (match extra
          [(list 'aoe-pict pict)
           (define svg (string->xexpr (bytes->string/utf-8 (convert pict 'svg-bytes))))
           `(li (span ([class "aoe"]) ,svg))]))))))

(define (monsters->xexprs group-id monsters mg env)
  (for/list ([monster monsters])
    (define id (~a "monster-" group-id "-" (monster-number monster)))
    (monster-xexpr id monster mg env)))

(define (monster-xexpr id m mg env)
  (define id-binding (list (~s "id") (~s id)))
  `(div ([id ,id])
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
        (p (select ([id ,(~a "select-conditions-" id)])
                   ,@(for/list ([c conditions])
                       `(option ([value ,(~a (discriminator:condition c))])
                                ,(~a c))))
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
                  ,@(~> (m) monster-conditions (active-conditions->xexpr id-binding "monster")))))))

(define (action-button actions bindings body [attrs empty])
  `(button ([type "button"]
            ,(action-click actions bindings)
            ,@attrs)
           ,body))

(define (active-conditions->xexpr cs id-binding [who "player"])
  (~> (cs)
      (map (flow (active-condition->xexpr id-binding who)) _)
      (add-between ", " #:before-last " and ")))

(define (active-condition->xexpr c id-binding [who "player"])
  `(span ([class "condition"])
         ,(~a c)
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

(define (get-pic name style)
  ((hash-ref (hasheq 'infused elements:element-pics-infused
                     'waning elements:element-pics-waning
                     'unfused elements:element-pics-unfused)
             style)
   ((hash-ref (hasheq 'Fire elements:fire 'Ice elements:ice 'Air elements:air
                      'Earth elements:earth 'Light elements:light 'Dark elements:dark)
              name))))

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
                      'player-initiative-input (hash 'value (~a (player-initiative p)))
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
      ['("initiative") (set-player-initiative req)]
      ['("loot") (loot! req)]
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
        (~>> ((list 'Fire 'Ice 'Air 'Earth 'Light 'Dark)
              (state-@elements (s)))
             (map cons) (assq e) cdr))
      (do (<@ @e-state transition-element-state))
      (response/empty)]
    [(or `(id . ,_) #f) (not-found req)]))

(define-flow (increment-player-hp req)
  (do-player player-at-max-health? (player-act-on-hp add1)))

(define-flow (decrement-player-hp req)
  (do-player player-dead? (player-act-on-hp sub1)))

(define-flow (increment-player-xp req)
  (do-player (const #f) (player-act-on-xp add1)))

(define-flow (decrement-player-xp req)
  (do-player (flow (~> player-xp zero?)) (player-act-on-xp sub1)))

(define-flow (remove-player-condition req)
  (do-player-condition #f))

(define-flow (add-player-condition req)
  (do-player-condition #t))

(define (set-player-initiative req)
  (match* {(req->player-id req) (req->initiative req)}
    [{(and id (not #f)) (and init (not #f))}
     (do (<~@ (state-@creatures (s))
              (update-players id (flow (player-set-initiative init)))))]
    [{_ _} (void)]))

(define (loot! req)
  (match (req->player-id req)
    [(and id (not #f)) (do ((give-player-loot (s)) id))]
    [_ (void)]))

(define/summon kill-summon (_r => [pid sid])
  (do-player/id pid (const #f) (player-kill-summon sid)))

(define/summon decrement-summon-hp (_r => [pid sid])
  (do-player/id pid
                (flow (~> player-summons (list-ref sid) summon-dead?))
                (update-player-summon sid (summon-act-on-hp sub1))))

(define/summon increment-summon-hp (_r => [pid sid])
  (do-player/id pid
                (flow (~> player-summons (list-ref sid) summon-at-max-health?))
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
  (do-monster-group/mgid mgid (monster-group-remove mn) (flow (~> 2> monster-group-first-monster))))

(define/monster decrement-monster-hp (_r => [mgid mn])
  (do-monster-group/n mgid mn (flow (switch [(not monster-dead?) (monster-update-hp sub1)]))))

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

(define (url-sans-param u)
  (struct-copy url u [path (map path/param-sans-param (url-path u))]))

(define (path/param-sans-param pp)
  (struct-copy path/param pp [param empty]))

(define (do-player req guard action)
  (match (req->player-id req)
    [(and id (not #f)) (do-player/id id guard action)]
    [_ (void)]))

(define (do-player/id id guard action)
  (do (<~@ (state-@creatures (s))
           (update-players id (flow (switch [(not guard) action]))))))

(define (do-player-condition req add-or-remove)
  (match* {(req->player-id req) (req->condition req)}
    [{(and id (not #f)) (and c (not #f))}
      (define c? (list c add-or-remove))
      (do (<~@ (state-@creatures (s))
               (update-players id (player-condition-handler c?))))]
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

(define (req->initiative r)
  (match (assq 'initiative (request-bindings r))
    [`(initiative . ,(app string->number (? (and/c number? initiative?) init)))
     init]
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

(define (do-monster-group/mgid mgid action [action-n (flow 1>)])
  (do (<~@ (state-@creatures (s))
           (update-monster-groups mgid action action-n))))

(define (do-monster-group/n mgid mn action)
  (do-monster-group/mgid mgid (monster-group-update-num mn action)))

(define (-do-monster req f)
  (match/values (req->monster-ids req)
    [{(and mgid (not #f)) (and mn (not #f))} (f req mgid mn)]
    [{_ _} (void)]))

(define-flow player-css-id (~a "player-" _))
(define-flow monster-group-css-id (~a "monster-group-" _))
(define-switch (creature-css-id c)
  (% creature-v creature-id)
  [player? player-css-id]
  [monster-group*? monster-group-css-id])
