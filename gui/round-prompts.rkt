#lang racket

(provide
 (contract-out
  [prompts-input-view (->* ((obs/c (listof prompt/c)))
                           (#:on-add (-> prompt/c any)
                            #:on-remove (-> natural-number/c prompt/c any))
                           (is-a?/c view<%>))]
  [manage-prompt-menu-item (->* ((obs/c (listof prompt/c)))
                                (#:on-add (-> prompt/c any)
                                 #:on-remove (-> natural-number/c prompt/c any))
                                (is-a?/c view<%>))]
  [do-round-prompt (-> time/c natural-number/c any)]))

(require racket/gui/easy
         racket/gui/easy/contract
         frosthaven-manager/manager/round-prompts
         frosthaven-manager/observable-operator
         frosthaven-manager/qi/utils
         frosthaven-manager/gui/mixins)

(define (prompts-input-view @prompts #:on-add [on-add void] #:on-remove [on-remove void])
  (define @keyed-prompts
    (@~> @prompts (~>> (-< (~> length range) _) (map cons))))
  (vpanel
   (button "Add Prompt"
           (thunk
            ;; not setting current renderer, nor using an eventspace: dialog
            (render (round-prompt-selector {(when _ on-add)}))))
   (list-view
     @keyed-prompts
     #:key car
     (λ (k @kp)
       (hpanel
        #:stretch '(#t #f)
        #:alignment '(center center)
        (text (@~> @kp (~> cdr prompt->string)))
        (button "X" (thunk (on-remove k (cdr (@! @kp))))))))))

(define (manage-prompt-menu-item @prompts #:on-add [on-add void] #:on-remove [on-remove void])
  (menu-item
   "Manage Round Prompts"
   (thunk
    ;; not setting current renderer, nor using an eventspace: dialog
    (render
     (dialog
      #:title "Manage Round Prompts"
      #:style '(close-button resize-border)
      #:size '(400 300)
      (prompts-input-view @prompts #:on-add on-add #:on-remove on-remove))))))


(define (do-round-prompt time round)
  (define-close! close! closing-mixin)
  ;; not setting current renderer, nor using an eventspace: dialog
  (render
   (dialog
    #:mixin closing-mixin
    #:title "Round Prompt"
    (text "Check the rules!")
    (text (~a "You asked to be reminded at the "
              (match time
                [(== beginning-of) "beginning of"]
                [(== end-of) "end of"])
              " round " round "."))
    (button "Ok" close!))))

;; on-finish: (-> (or/c #f prompt/c) any)
(define (round-prompt-selector [on-finish void])
  (define/obs @time beginning-of)
  (define/obs @rule "round:")
  (define/obs @m #f)
  (define/obs @n #f)
  (define/obs @start #f)
  (define @prompt
    (obs-combine
     (λ (time rule m n start)
       (and time
            rule
            (case rule
              [("round:") (and m (first (prompt [time m])))]
              [("every N rounds") (and n start (first (prompt [time every n starting-at start])))]
              [("every even round") (first (prompt [time even]))]
              [("every odd round") (first (prompt [time odd]))])))
     @time @rule @m @n @start))
  (define-close! close! closing-mixin)
  (dialog
   #:title "Round Prompt"
   #:mixin (compose1 closing-mixin (make-on-close-mixin (thunk (on-finish (@! @prompt)))))
   (hpanel
    (choice (list beginning-of end-of)
            (λ:= @time)
            #:choice->label (λ (c)
                              (cond
                                [(equal? c beginning-of) "Beginning of"]
                                [(equal? c end-of) "End of"]))
            #:selection beginning-of)
    (choice (list "round:" "every even round" "every odd round" "every N rounds")
            (λ:= @rule))
    (case-view @rule
      [("round:") (input (->input-value @m) (->input-handler close! @m))]
      [("every N rounds") (hpanel
                           (input (->input-value @n) (->input-handler close! @n))
                           (input (->input-value @start) (->input-handler close! @start) #:label "starting at"))]
      [else (spacer)]))
   (hpanel
    (button "Ok" close!)
    (button "Cancel" (thunk (:= @time #f) (close!))))))

(define (->input-value @N)
  (@~> @N (if _ ~a "")))

(define ((->input-handler close! @N) event str)
  (when (equal? event 'return)
    (close!))
  (cond [(string->number str) => (λ:= @N)]))

(module+ main
  (define/obs @prompts empty)
  (define (add p)
    (<~@ @prompts (append (list p))))
  (define (remove i p)
    (define-values (new-ps p2)
      (list-remove (@! @prompts) i))
    (when (equal? p p2)
      (:= @prompts new-ps)))
  (render
   (window
    #:title "Round Prompts"
    (prompts-input-view @prompts #:on-add add #:on-remove remove))))
