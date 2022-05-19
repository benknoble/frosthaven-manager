#lang racket/base

(module+ main
  (require "gui.rkt")
  ;; (require racket/gui/easy/debugger)
  ;; (start-debugger)
  (void (render-manager)))
