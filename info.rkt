#lang info

(define collection "frosthaven-manager")
(define racket-launcher-names '("frosthaven-manager"))
(define racket-launcher-libraries '("main"))
(define gracket-launcher-names '("FrosthavenManager"))
(define gracket-launcher-libraries '("main"))
(define deps '("markdown"
               "net-lib"
               "txexpr"
               "gui-lib"
               "htdp-lib"
               "pict-lib"
               "qi-lib"
               "gui-easy-lib"
               "rebellion"
               "base"))
(define build-deps '("rackunit-lib"
                     "scribble-lib"
                     "racket-doc"))
(define scribblings '(("scribblings/frosthaven-manager.scrbl" ())))
(define pkg-desc "Frosthaven Scenario Manager")
(define version "0.0")
(define pkg-authors '(benknoble))
(define license 'MIT)
