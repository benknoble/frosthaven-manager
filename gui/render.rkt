#lang racket/base

(require racket/contract
         racket/gui/easy)
(provide
  (contract-out
    [current-renderer (parameter/c (or/c #f renderer?))]
    [render/eventspace (->* ((is-a?/c view<%>))
                            (#:parent (or/c #f renderer?)
                             #:eventspace eventspace?)
                            renderer?)])
  with-closing-custodian/eventspace
  closing-custodian closing-eventspace close-custodian-mixin)

(require (for-syntax racket/base)
         racket/gui
         racket/stxparam
         syntax/parse/define)

(define current-renderer (make-parameter #f))

(define (render/eventspace tree #:parent [parent #f] #:eventspace [es (current-eventspace)])
  (parameterize ([current-eventspace es])
    (define r (render tree parent))
    ;; set current-renderer in handler-thread of es
    (queue-callback (thunk (current-renderer r)) 'high-priority)
    r))

(define-syntax-parse-rule (with-closing-custodian/eventspace body:expr ...+)
  (syntax/loc this-syntax
    (let* ([cust (make-custodian)]
           [es (parameterize ([current-custodian cust])
                 (make-eventspace))]
           [shutdown-cust (shutdown-on-close cust)])
      (syntax-parameterize ([closing-custodian (make-rename-transformer #'cust)]
                            [closing-eventspace (make-rename-transformer #'es)]
                            [close-custodian-mixin (make-rename-transformer #'shutdown-cust)])
        body ...))))

(begin-for-syntax
  (define (only-in-with-closing-custodian/eventspace stx)
    (raise-syntax-error #f "can only be used inside with-closing-custodian/eventspace" stx)))
(define-syntax-parameter closing-custodian only-in-with-closing-custodian/eventspace)
(define-syntax-parameter closing-eventspace only-in-with-closing-custodian/eventspace)
(define-syntax-parameter close-custodian-mixin only-in-with-closing-custodian/eventspace)

(define (shutdown-on-close cust)
  (mixin (top-level-window<%>) ()
    (super-new)
    (define/augment (on-close)
      ;; shutdown the custodian after processing remaining events
      (queue-callback (thunk (custodian-shutdown-all cust))))))
