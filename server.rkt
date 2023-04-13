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
  frosthaven-manager/elements
  frosthaven-manager/gui/elements
  frosthaven-manager/manager
  frosthaven-manager/observable-operator
  json
  nat-traversal
  net/url
  racket/gui
  racket/gui/easy
  racket/runtime-path
  web-server/dispatch/syntax
  #;web-server/dispatch/url-patterns
  web-server/dispatch/extend
  web-server/dispatchers/filesystem-map
  web-server/http
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

(define (launch-server an-s a-send-event)
  ;; each request thread get its own receiver, so that they can all see the
  ;; updates
  (define ch (make-multicast-channel))
  (for ([e (elements)]
        [@e-state (state-@elements an-s)])
    (obs-observe!
      @e-state
      (λ (new-e-state)
        (multicast-channel-put ch `(element ,(element-pics-name e) ,new-e-state)))))

  (define-values (app the-reverse-uri)
    (dispatch-rules
      [("") overview]
      [("events") (event-source ch)]
      [("element-pics" (element-name-arg) (element-style-arg)) element-pic]
      [else not-found]))

  (define url->path/static
    (make-url->path static))

  (define static-dispatcher
    (files:make #:url->path (λ (u)
                              (url->path/static
                                (struct-copy url u [path (cdr (url-path u))])))))

  (define port 8001)
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
         ))))

(define (elements-body embed/url)
  `((h2 "Elements")
    ,@(for/list ([e (list 'Fire 'Ice 'Air 'Earth 'Light 'Dark)]
                 [@e-state (state-@elements (s))])
        `(a ([href
               ,(embed/url
                  (λ (_req)
                    ((send-event)
                      (λ ()
                        (<@ @e-state transition-element-state)))
                    (redirect-to "/" see-other)))])
            (img ([id ,(symbol->string e)]
                  [src ,((reverse-uri) element-pic e (@! @e-state))]))))))

(define (get-pic name style)
  ((hash-ref (hasheq 'infused element-pics-infused
                     'waning element-pics-waning
                     'unfused element-pics-unfused)
             style)
   ((hash-ref (hasheq 'Fire fire 'Ice ice 'Air air
                      'Earth earth 'Light light 'Dark dark)
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
      (display "\n\n" out)]))

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
