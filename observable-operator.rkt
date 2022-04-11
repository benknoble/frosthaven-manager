#lang racket

(provide (rename-out [<~ <@]
                     [~> @>]
                     [λ<~ λ<@]
                     [obs-peek @!])
         define/obs @ := λ:=
         @~> <~@)

(require racket/gui/easy
         racket/gui/easy/operator
         syntax/parse/define
         (only-in qi flow))

(define-syntax-parse-rule (@~> @o:expr flo:expr)
  (~> @o (flow flo)))

(define-syntax-parse-rule (<~@ @o:expr flo:expr)
  (<~ @o (flow flo)))
