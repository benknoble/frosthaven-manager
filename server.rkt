#lang racket

; vim: lw+=define-bidi-match-expander,define-bidi-match-expander/coercions,page,define/page,lambda/page

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
  web-server/dispatch/syntax
  web-server/dispatch/url-patterns
  web-server/dispatch/extend
  web-server/dispatchers/filesystem-map
  web-server/http
  web-server/http/bindings
  web-server/managers/lru
  web-server/page
  web-server/servlet-dispatch
  #;web-server/servlet/web
  web-server/web-server
  )

(define-runtime-path static "static")

(define s (make-parameter #f))
(define reverse-uri (make-parameter #f))
(define send-event (make-parameter #f))

(define-syntax-rule (do e ...)
  ((send-event)
   (thunk e ...)))

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
      (for ([c creatures])
        (cond
          [(player? (creature-v c)) (multicast-channel-put ch `(player ,c))]))))

  (define-values (app the-reverse-uri)
    (dispatch-rules
      [("") overview]
      [("action" "player" (string-arg) (string-arg) ...) #:method "post" player-action]
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
         ))))

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
  `((h2 "Players")
    (ul
      ([class "players"])
      ,@(for/list ([c (@! (state-@creatures (s)))]
                   #:when (player? (creature-v c)))
          (define p (creature-v c))
          (define id-binding (list (~s "id") (~s (~s (creature-id c)))))
          `(li ([id ,(~a "player-" (creature-id c))])
               (span ([class "player-name"])
                     ,(player-name p))
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
                     "+"))
               (p (span ([class "player-conditions"])
                        ,@(~> (p) player-conditions* (map condition->xexpr _)
                              (add-between ", " #:before-last " and ")))))))))

(define (get-pic name style)
  ((hash-ref (hasheq 'infused elements:element-pics-infused
                     'waning elements:element-pics-waning
                     'unfused elements:element-pics-unfused)
             style)
   ((hash-ref (hasheq 'Fire elements:fire 'Ice elements:ice 'Air elements:air
                      'Earth elements:earth 'Light elements:light 'Dark elements:dark)
              name))))

(define (element-pic _req name style)
  (response/output
    (λ (o)
      (write-bytes (convert (get-pic name style) 'svg-bytes) o))
    #:mime-type #"image/svg+xml"))

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
      (define css-id (~a "player-" id))
      (define data
        (hash 'id css-id
              'data (hash
                      'player-name (player-name p)
                      'player-HP (player->hp-text p)
                      'player-XP (~a (player-xp p))
                      'player-conditions
                      (~> (p) player-conditions* (map ~a _)
                          (string-join ", " #:before-last " and ")))))
      (displayln "event: player" out)
      (display (format "data: ~a" (jsexpr->string data)) out)
      (displayln "\n\n" out)]))

(define (not-found _req)
  (response/xexpr
    #:code 404
    `(html
       (head
         (title "Not Found")
         ,@common-heads)
       (body
         (h1 "Not Found")))))

(define (expired-page req)
  (response/xexpr
    `(html
       (head
         (title "Page Has Expired.")
         ,@common-heads)
       (body
         (p "Sorry, this page has expired."
            (a ([href ,(url->string (url-sans-param (request-uri req)))])
               "This page")
            " may be the one you wanted.")))))

(define common-heads
  `((meta ([name "viewport"] [content "width=device-width, initial-scale=1.0"]))))

(define (url-sans-param u)
  (struct-copy url u [path (map path/param-sans-param (url-path u))]))

(define (path/param-sans-param pp)
  (struct-copy path/param pp [param empty]))

(define (player-action req what args)
  (let/ec return
    (match (cons what args)
      ['("hp" "+") (increment-player-hp req)]
      ['("hp" "-") (decrement-player-hp req)]
      ['("xp" "+") (increment-player-xp req)]
      ['("xp" "-") (decrement-player-xp req)]
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

(define (increment-player-hp req)
  (do-player req player-at-max-health? (player-act-on-hp add1)))

(define (decrement-player-hp req)
  (do-player req player-dead? (player-act-on-hp sub1)))

(define (increment-player-xp req)
  (do-player req (const #f) (player-act-on-xp add1)))

(define (decrement-player-xp req)
  (do-player req (flow (~> player-xp zero?)) (player-act-on-xp sub1)))

(define (do-player req guard action)
  (match (assq 'id (request-bindings req))
    [`(id . ,(app string->number (? number? id)))
      (do (<~@ (state-@creatures (s))
               (update-players id (flow (switch [(not guard) action])))))]
    [#f (void)]))

(define (action-button actions bindings body [attrs empty])
  `(button ([type "button"]
            ,(action-click actions bindings)
            ,@attrs)
           ,body))

(define (action-click actions bindings)
  (define URL (string-join (cons "action" actions) "/" #:before-first "/"))
  (define params
    (string-join
      (for/list ([b bindings])
        (match-define (list key value) b)
        (format "[~a, ~a]" key value))
      ","))
  `[onclick ,(format "fetch('~a', {method: 'POST', body: new URLSearchParams([~a])})"
                     URL params)])

(define (condition->xexpr c)
  (~a c))
