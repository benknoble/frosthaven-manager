#lang racket

(provide (rename-out [gui:<~ <@]
                     [gui:~> @>]
                     [gui:λ<~ λ<@]
                     [gui:obs-peek @!]
                     [gui:define/obs define/obs]
                     [gui:@ @]
                     [gui::= :=]
                     [gui:λ:= λ:=])
         (all-from-out frosthaven-manager/curlique))

(require (prefix-in gui: (combine-in racket/gui/easy
                                     racket/gui/easy/operator))
         frosthaven-manager/curlique)
