#lang racket

(provide
  (contract-out
    [do-about (-> renderer?)]
    [about-menu-item (-> (is-a?/c view<%>))]
    [issue-menu-item (-> (is-a?/c view<%>))]
    [feature-menu-item (-> (is-a?/c view<%>))]
    [contribute-menu-item (-> (is-a?/c view<%>))]
    [send-feedback-menu-item (-> (is-a?/c view<%>))]
    [how-to-play-menu-item (-> (is-a?/c view<%>))]
    [launch-server-menu-item (-> state? (is-a?/c view<%>))]
    [gc-menu-item (-> (is-a?/c view<%>))]))

(require frosthaven-manager/gui/markdown
         frosthaven-manager/gui/render
         frosthaven-manager/gui/server
         frosthaven-manager/manager
         net/sendurl
         racket/gui/easy
         racket/runtime-path
         (prefix-in gui: racket/gui))

(module+ test (require rackunit))

(begin-for-syntax
  (require setup/getinfo)
  (define (get-version) ((get-info '("frosthaven-manager")) 'version)))
(define-syntax (version stx)
  (datum->syntax stx (get-version) stx stx))

(module+ test
  (require version/utils)
  (define-simple-check (check-valid-version x)
                       (valid-version? x))
  (check-valid-version version))

(define-runtime-path about.md "../ABOUT.md")

(define (get-about-text)
  (define about-text (file->string about.md))
  (string-join (list about-text "---" (string-append "Version: " version)) "\n"))

(module+ test
  (test-case "about information"
    (check-not-exn get-about-text)
    (check-match (get-about-text) (regexp #px"Version: (.*)" (list _ the-version))
                 (valid-version? the-version))))

(define (do-about)
  (with-closing-custodian/eventspace
    (render/eventspace
      #:eventspace closing-eventspace
      (window
        #:mixin close-custodian-mixin
        #:title "About Frosthaven Manager"
        #:size '(400 300)
        (markdown-text (get-about-text))))))

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
  (menu-item "How to Play" (thunk (send-url "https://benknoble.github.io/frosthaven-manager/How_to_play.html"))))

(define (launch-server-menu-item s)
  (menu-item "Launch Server" (thunk (launch-server s))))

(define (gc-menu-item)
  (menu-item "Observe GC"
             (thunk
              (with-closing-custodian/eventspace
               (render/eventspace
                #:eventspace closing-eventspace
                (window
                 #:mixin close-custodian-mixin
                 #:title "GC"
                 #:size '(120 120)
                 (canvas
                  (obs #f)
                  void
                  #:mixin (mixin (gui:canvas<%>) (gui:canvas<%>)
                            (super-new)
                            (gui:register-collecting-blit
                             this
                             0 0
                             80 80
                             (gui:make-monochrome-bitmap 80 80 (make-bytes (sqr 80) #xff))
                             (gui:make-monochrome-bitmap 80 80))))))))))

(module+ main
  (render
    (window
      (text "Check menus")
      (menu-bar
        (menu "Help"
              (about-menu-item)
              (how-to-play-menu-item)
              (menu-item-separator)
              (send-feedback-menu-item)
              (issue-menu-item)
              (feature-menu-item)
              (contribute-menu-item))))))
