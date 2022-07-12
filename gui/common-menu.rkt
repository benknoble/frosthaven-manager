#lang racket

(provide
  (contract-out
    [about-menu-item (-> (is-a?/c view<%>))]
    [issue-menu-item (-> (is-a?/c view<%>))]
    [feature-menu-item (-> (is-a?/c view<%>))]
    [contribute-menu-item (-> (is-a?/c view<%>))]
    [send-feedback-menu-item (-> (is-a?/c view<%>))]))

(require racket/runtime-path
         net/sendurl
         racket/gui/easy
         "markdown.rkt")

(define-runtime-path about.md "../ABOUT.md")

(define (about-menu-item)
  (menu-item
    "About Frosthaven Manager"
    (thunk
      (render
        (window
          #:title "About Frosthaven Manager"
          #:size '(400 300)
          (markdown-text about.md))))))

(define (issue-menu-item)
  (menu-item
    "Report an Issue"
    (thunk (send-url "https://github.com/benknoble/frosthaven-manager/issues/new/choose"))))

(define (feature-menu-item)
  (menu-item
    "Request a Feature"
    (thunk (send-url "https://github.com/benknoble/frosthaven-manager/issues/new/choose"))))

(define (contribute-menu-item)
  (menu-item
    "Contribute to development"
    (thunk (send-url "https://github.com/benknoble/frosthaven-manager"))))

(define (send-feedback-menu-item)
  (menu-item
    "Send Feedback"
    (thunk (send-url "mailto:ben.knoble+frosthaven@gmail.com"))))
