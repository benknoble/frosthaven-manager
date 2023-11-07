#lang racket

(provide (rename-out [gui:<~ <@]
                     [gui:~> @>]
                     [gui:λ<~ λ<@]
                     [gui:obs-peek @!]
                     [gui:define/obs define/obs]
                     [gui:@ @]
                     [gui::= :=]
                     [gui:λ:= λ:=])
         @~> <~@
         (all-from-out qi))

(require (prefix-in gui: (combine-in racket/gui/easy
                                     racket/gui/easy/operator))
         syntax/parse/define
         qi)

(define-syntax-parse-rule (@~> @o:expr flo:expr)
  (gui:~> @o (flow flo)))

(define-syntax-parse-rule (<~@ @o:expr flo:expr)
  (gui:<~ @o (flow flo)))
