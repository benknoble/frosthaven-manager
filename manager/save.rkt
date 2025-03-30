#lang racket

(provide
 (contract-out
  [do-save-game (-> state? any)]
  [do-load-game (-> state? any)]
  [save-game (-> state? (-> path-string? any))]
  [load-game (-> state? (-> path-string? any))]))

(require frosthaven-manager/files
         frosthaven-manager/manager/state
         frosthaven-manager/observable-operator)

(define ((save-game s) p)
  (call-with-output-file* p (curry serialize-state s) #:exists 'replace))

(define (do-save-game s)
  (cond [(put-file/filter "Save Game" '("Saved Games" "*.fasl")) => (save-game s)]))

(define ((load-game s) p)
  (define saved-state (call-with-input-file* p deserialize-state))
  (copy-state saved-state s)
  ;; Restore error logs, unless there was no file but now stderr isn't hooked up
  ;; to a TTY. See gui/manager.rkt startup sequence.
  (cond
    [(@! (state-@error-logs s))
     =>
     (λ (logs)
       (current-error-port (open-output-file logs #:exists 'append #:mode 'text)))]
    [(not (terminal-port? (current-error-port)))
     (define temp (make-temporary-file "frosthaven-manager-~a"))
     (current-error-port (open-output-file temp #:exists 'truncate #:mode 'text))
     (:= (state-@error-logs s) temp)]))

(define (do-load-game s)
  (cond [(get-file/filter "Load Game" '("Saved Games" "*.fasl")) => (load-game s)]))
