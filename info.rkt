#lang info

(define collection "frosthaven-manager")
(define deps '("gui-easy-lib"
               "rebellion"
               "base"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/frosthaven-manager.scrbl" ())))
(define pkg-desc "Frosthaven Scenario Manager")
(define version "0.0")
(define pkg-authors '(benknoble))
(define license '(MIT))
