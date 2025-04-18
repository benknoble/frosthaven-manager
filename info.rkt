#lang info

(define collection "frosthaven-manager")
(define racket-launcher-names '("frosthaven-manager"))
(define racket-launcher-libraries '("gui/manager"))
(define gracket-launcher-names '("FrosthavenManager"))
(define gracket-launcher-libraries '("gui/manager"))
(define deps '("dbg"
               "syntax-classes-lib"
               "snip-lib"
               "pict-snip-lib"
               "pretty-expressive"
               "alexis-multicast"
               "nat-traversal"
               "web-server-lib"
               "functional-lib"
               ("megaparsack-lib" #:version "1.8")
               "reprovide-lang-lib"
               "draw-lib"
               "markdown"
               "net-lib"
               "txexpr"
               "gui-lib"
               "htdp-lib"
               "pict-lib"
               ("qi-lib" #:version "4.0")
               ("gui-easy-lib" #:version "0.18")
               "rebellion"
               "base"))
(define build-deps '("draw-doc"
                     "pict-doc"
                     "gui-doc"
                     "gui-easy"
                     "megaparsack-doc"
                     ("qi-doc" #:version "4.0")
                     "rackunit-lib"
                     "scribble-lib"
                     "racket-doc"))
(define scribblings '(("scribblings/frosthaven-manager.scrbl" (multi-page))))
(define pkg-desc "Frosthaven Scenario Manager")
(define version "0.27")
(define pkg-authors '(benknoble))
(define license 'MIT)

(define compile-omit-paths '("FrosthavenManager.app"
                             "macOS-FrosthavenManager"
                             "linux-FrosthavenManager"
                             "windows-FrosthavenManager"
                             "docs"
                             "screenshots"))
(define test-omit-paths compile-omit-paths)
