#lang info

(define collection "frosthaven-manager")
(define racket-launcher-names '("frosthaven-manager"))
(define racket-launcher-libraries '("gui/manager"))
(define gracket-launcher-names '("FrosthavenManager"))
(define gracket-launcher-libraries '("gui/manager"))
(define deps '("functional-lib"
               "megaparsack-lib"
               "reprovide-lang-lib"
               "plot-lib"
               "draw-lib"
               "markdown"
               "net-lib"
               "txexpr"
               "gui-lib"
               "htdp-lib"
               "pict-lib"
               ("qi-lib" #:version "3.0")
               ("gui-easy-lib" #:version "0.5")
               "rebellion"
               "base"))
(define build-deps '("gui-doc"
                     "gui-easy"
                     "megaparsack-doc"
                     ("qi-doc" #:version "3.0")
                     "rackunit-lib"
                     "scribble-lib"
                     "racket-doc"))
(define scribblings '(("scribblings/frosthaven-manager.scrbl" (multi-page))))
(define pkg-desc "Frosthaven Scenario Manager")
(define version "0.5.2")
(define pkg-authors '(benknoble))
(define license 'MIT)

(define compile-omit-paths '("FrosthavenManager.app"
                             "macOS-FrosthavenManager"
                             "linux-FrosthavenManager"
                             "windows-FrosthavenManager"
                             "docs"))
