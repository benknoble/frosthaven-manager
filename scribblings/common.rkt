#lang racket/base

(provide markdown-inline
         markdown-part
         ABOUT.md

         terminal)

(require racket/match
         racket/runtime-path
         scribble/base
         scribble/decode
         markdown
         markdown/scrib)

(define-runtime-path ABOUT.md "../ABOUT.md")

(define (markdown-inline file)
  (xexprs->scribble-pres
    (strip-html-comments
      (with-input-from-file file read-markdown))))

(define (markdown-part file)
  (decode
    (xexprs->scribble-pres
      (strip-html-comments
        (with-input-from-file file read-markdown)))))

(define (strip-html-comments xexpr)
  (match xexpr
    [(cons '!HTML-COMMENT _) ""]
    [(list tag (list (list attr val) ...) x ...)
     (list* tag (map list attr val)
            (map strip-html-comments x))]
    [(list tag x ...) (list* tag (map strip-html-comments x))]
    [else xexpr]))

(define (terminal . args)
  (nested #:style 'code-inset
          (apply verbatim args)))
