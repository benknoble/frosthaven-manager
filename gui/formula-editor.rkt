#lang racket

(provide
  (contract-out
    [formula-editor (-> (obs/c env/c) (is-a?/c view<%>))]
    [formula-menu-item (-> (obs/c env/c) (is-a?/c view<%>))]))

(require racket/gui/easy
         racket/gui/easy/contract
         megaparsack
         frosthaven-manager/observable-operator
         frosthaven-manager/parsers/formula
         frosthaven-manager/gui/render)

(define (formula-editor @env #:mix [mix values])
  (define/obs @input "")
  (define @result
    (obs-combine
      (λ (inp env)
        (with-handlers ([exn:fail:read:megaparsack?
                          (λ (exn)
                            (parse-error->string
                              (message (match (exn:fail:read-srclocs exn)
                                         [(cons (srcloc _ line column posn span) _)
                                          (srcloc "Formula" line column posn span)]
                                         [_ (make-srcloc "Formula" #f #f #f #f)])
                                       (exn:fail:read:megaparsack-unexpected exn)
                                       (exn:fail:read:megaparsack-expected exn))))]
                        [exn:fail:contract? (const "reference to unavailable variable")])
          ((parse-expr inp) env)))
      @input @env))
  (define (@var-display var)
    (@~> @env (~> (hash-ref var #f)
                  (or _ "unavailable")
                  (~a var ": " _))))
  (window
    #:title "Formula editor"
    #:mixin mix
    #:size '(300 200)
    (vpanel
      (input @input (λ (_evt inp) (:= @input inp))
             #:style '(multiple)
             #:label "Formula")
      (input (@> @result ~a) void
             #:enabled? #f
             #:style '(multiple)
             #:label "Results")
      (hpanel
        (vpanel
          (text "Variables")
          (text (@var-display "L"))
          (text (@var-display "C")))
        (vpanel
          (text "Operators")
          (text "+ Add")
          (text "- Subtract")
          (text "* Multiply")
          (text "/ Divide"))))))

(define (formula-menu-item @env)
  (menu-item "Formula Editor"
             (thunk (with-closing-custodian/eventspace
                      (render/eventspace
                        #:eventspace closing-eventspace
                        (formula-editor @env #:mix close-custodian-mixin))))))

(module+ main
  (define/obs @c 4)
  (define/obs @c? #t)
  (define/obs @l 2)
  (define/obs @l? #t)
  (render
    (window
      (hpanel
        (checkbox (λ:= @l?) #:checked? @l?)
        (slider #:label "L" @l (λ:= @l) #:min-value 0 #:max-value 7))
      (hpanel
        (checkbox (λ:= @c?) #:checked? @c?)
        (slider #:label "C" @c (λ:= @c) #:min-value 2 #:max-value 4))
      (menu-bar (menu "File" (formula-menu-item
                               (obs-combine
                                 (λ (c c? l l?)
                                   (make-immutable-hash
                                     (append (if c? (list (cons "C" c)) null)
                                             (if l? (list (cons "L" l)) null))))
                                 @c @c? @l @l?)))))))
