#lang racket
; vim: lw-=do

(provide (all-from-out megaparsack
                       megaparsack/text
                       data/monad
                       data/applicative
                       data/functor
                       frosthaven-manager/defns
                       frosthaven-manager/qi)
         (contract-out
           [string-ci/p (-> string? (parser/c char? string?))]
           [skip-ws (parser/c char? void?)]
           [opt/p (-> (parser/c char? any/c) (parser/c char? any/c))]
           [list-value/p (-> (parser/c char? any/c) (parser/c char? list?))]
           [struct labelled ([label any/c]
                             [v any/c])]
           [labelled/p (-> (parser/c char? any/c)
                           (parser/c char? any/c)
                           (parser/c char? labelled?))]
           [map/p (-> (parser/c char? any/c)
                      (parser/c char? any/c)
                      (parser/c char? (and/c immutable? hash?)))]
           [text/p (parser/c char? string?)]
           [number/p (parser/c char? integer?)]
           [non-empty-text/p (-> string? (parser/c char? string?))]
           [set-name?/p (-> string? (parser/c char? string?))]
           [ws-separated-whole-file/p (-> (parser/c char? any/c) (parser/c char? list?))]
           [make-reader-like (-> (parser/c char? any/c)
                                 (-> any/c input-port? #:syntax? any/c any/c))]))

(require megaparsack
         megaparsack/text
         data/monad
         data/applicative
         (rename-in data/functor
                    [map fmap])
         frosthaven-manager/defns
         frosthaven-manager/qi)

;; megaparsack's string-ci/p is broken
;; (https://github.com/lexi-lambda/megaparsack/issues/24)
(define (string-ci/p s)
  (if (zero? (string-length s))
    (pure "")
    (label/p s (do (char-ci/p (string-ref s 0))
                   (string-ci/p (substring s 1))
                   (pure s)))))

(define skip-ws
  (do (many/p space/p)
      void/p))

(define (opt/p p)
  (do (char/p #\() skip-ws
      [x <- p] skip-ws
      (char/p #\))
      (pure x)))

(define (list-value/p p)
  (do (char/p #\{) skip-ws
      [v <- (many/p p #:sep skip-ws)] skip-ws
      (char/p #\})
      (pure v)))

(struct labelled [label v])
(define (labelled/p label/p p)
  (do (char/p #\[) skip-ws
      [label <- label/p] skip-ws
      [v <- p] skip-ws
      (char/p #\])
      (pure (labelled label v))))

(define (map/p key/p value/p)
  (do (char/p #\<) skip-ws
      [kvps <- (many/p (labelled/p key/p value/p) #:sep skip-ws)] skip-ws
      (char/p #\>)
      (pure (~> (kvps) sep (>< (-< labelled-label labelled-v)) hash))))

(define text/p
  (label/p "text"
           (do (char/p #\")
               [text <- (fmap list->string (many/p (char-not/p #\")))]
               (char/p #\")
               (pure text))))

(define number/p
  (label/p "number"
           (do [sign <- (or/p (fmap (const -) (char/p #\-))
                              (pure +))]
               [number <- integer/p]
               (pure (sign number)))))

(define non-empty-string? (not/c (string-len/c 1)))
(define (non-empty-text/p why)
  (guard/p text/p non-empty-string? why))

(define-flow name->set (~> string-split (and (~> length (> 0)) last)))

(define (set-name?/p name)
  (or/p (opt/p (non-empty-text/p "non-empty set name"))
        (guard/p
          (~> (name) name->set pure)
          (flow (and _ (~> string-length (not zero?))))
          "name with monster set"
          (const name))))

(define (ws-separated-whole-file/p p)
  (fmap first
        (do skip-ws
          (many-until/p p
                        #:sep skip-ws
                        #:end (try/p (do skip-ws eof/p))))))

(define ((make-reader-like p) src in #:syntax? syntax?)
  (define wrapper (if syntax? syntax/p identity))
  (parse-result! (parse-string (wrapper p) (port->string in) src)))
