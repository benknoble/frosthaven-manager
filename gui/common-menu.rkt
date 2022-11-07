#lang racket

(provide
  (contract-out
    [do-about (-> renderer?)]
    [about-menu-item (-> (is-a?/c view<%>))]
    [issue-menu-item (-> (is-a?/c view<%>))]
    [feature-menu-item (-> (is-a?/c view<%>))]
    [contribute-menu-item (-> (is-a?/c view<%>))]
    [send-feedback-menu-item (-> (is-a?/c view<%>))]
    [how-to-play-menu-item (-> (is-a?/c view<%>))]))

(require racket/runtime-path
         net/sendurl
         racket/gui/easy
         frosthaven-manager/gui/markdown
         frosthaven-manager/gui/render)

(define-runtime-path about.md "../ABOUT.md")

(define (do-about)
  (with-closing-custodian/eventspace
    (render/eventspace
      #:eventspace closing-eventspace
      (window
        #:mixin close-custodian-mixin
        #:title "About Frosthaven Manager"
        #:size '(400 300)
        (markdown-text about.md)))))

(define (about-menu-item)
  (menu-item "About Frosthaven Manager" do-about))

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

(define (how-to-play-menu-item)
  (menu-item "How to Play" (thunk (send-url "https://benknoble.github.io/frosthaven-manager/How_to_Play.html"))))
