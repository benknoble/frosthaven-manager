#lang info

(define collection "frosthaven-manager")
(define racket-launcher-names '("frosthaven-manager"))
(define racket-launcher-libraries '("manager"))
(define gracket-launcher-names '("FrosthavenManager"))
(define gracket-launcher-libraries '("manager"))
(define deps '("markdown"
               "net-lib"
               "txexpr"
               "gui-lib"
               "htdp-lib"
               "pict-lib"
               "qi-lib"
               ("gui-easy-lib" #:version "0.5")
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
