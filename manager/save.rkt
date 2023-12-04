#lang racket

(provide
 (contract-out
  [do-save-game (-> state? any)]
  [do-load-game (-> state? any)]
  [load-game (-> state? (-> path-string? any))]))

(require frosthaven-manager/files
         frosthaven-manager/manager/state)

(define ((save-game s) p)
  (call-with-output-file* p (curry serialize-state s) #:exists 'replace))

(define (do-save-game s)
  (cond [(put-file/filter "Save Game" '("Saved Games" "*.fasl")) => (save-game s)]))

(define ((load-game s) p)
  (define saved-state (call-with-input-file* p deserialize-state))
  (copy-state saved-state s))

(define (do-load-game s)
  (cond [(get-file/filter "Load Game" '("Saved Games" "*.fasl")) => (load-game s)]))
