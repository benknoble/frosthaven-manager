#lang racket

(provide
  #%app #%datum #%top #%top-interaction
  (rename-out [mb #%module-begin])
  extend-standard-deck
  sticker
  money
  lumber metal hide
  arrowvine axenut corpsecap flamefruit rockroot snowthistle)

(require syntax/parse/define
         racket/hash
         (for-syntax racket/syntax)
         frosthaven-manager/defns)

(module reader syntax/module-reader
  frosthaven-manager/loot-cards)

(define-syntax-parse-rule (mb e:expr ...)
  #:with result (format-id this-syntax "loot-cards" #:source this-syntax)
  (#%module-begin
   (provide result)
   (define result
     (for/fold ([x (hash)])
               ([f (list e ...)])
       (f x)))))

(define-syntax-parser extend-standard-deck
  [_ #'(-extend-standard-deck)]
  [(_) #'(-extend-standard-deck)])

(begin-for-syntax
 (define-syntax-class money-spec
   #:attributes {constructor}
   #:literals {money}
   [pattern [money amount:number]
            #:with constructor #'(money amount)])
 (define-syntax-class material-spec
   #:attributes {constructor}
   #:literals {lumber metal hide}
   [pattern [{~and t {~or lumber metal hide}}
             2player-amount:number
             3player-amount:number
             4player-amount:number]
            #:with constructor #'(material t (list 2player-amount 3player-amount 4player-amount))])
 (define-syntax-class herb-spec
   #:attributes {constructor}
   #:literals {arrowvine axenut corpsecap flamefruit rockroot snowthistle}
   [pattern {~and t {~or arrowvine axenut corpsecap flamefruit rockroot snowthistle}}
            #:with constructor #'(herb t 1)]
   [pattern [{~and t {~or arrowvine axenut corpsecap flamefruit rockroot snowthistle}} amount:number]
            #:with constructor #'(herb t amount)])
 (define-syntax-class card-spec
   #:attributes {constructor}
   [pattern m:money-spec #:with constructor #'m.constructor]
   [pattern m:material-spec #:with constructor #'m.constructor]
   [pattern h:herb-spec #:with constructor #'h.constructor]))

(define-syntax-parse-rule (sticker [stickers:number c:card-spec] ...)
  (-sticker (list (cons stickers c.constructor) ...)))

(define (-extend-standard-deck)
  (const
   (hash-union (hash money money-deck 'random-item (list random-item))
               material-decks
               herb-decks)))

(define ((-sticker stickers-per-card) x)
  (let loop ([res (hash)]
             [x x]
             [stickers-per-card stickers-per-card])
    (match stickers-per-card
      ['() (hash-union res x #:combine append)]
      [(cons (cons n card) stickers-per-card)
       (define type (card->type card))
       (define old-card
         (match (member card (hash-ref x type))
           [(cons old-card _) old-card]
           [_ (raise-user-error 'sticker
                                "card (~a ~a) does not exist in deck"
                                type
                                (match card
                                  [(money amount) amount]
                                  [(material _ amounts) (string-join (map ~a amounts))]
                                  [(herb _ amount) amount]))]))
       (define x*
         (hash-update x type (λ (old) (remove card old))))
       (define new
         (for/fold ([card old-card])
                   ([_i (in-range n)])
           (apply-sticker card)))
       (define res*
         (hash-update res type (λ (old) (cons new old)) '()))
       (loop res* x* stickers-per-card)])))
