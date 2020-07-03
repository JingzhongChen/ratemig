#lang racket/base

(require racket/cmdline
         ratemig/commands/private/gen)

(define main
  (command-line
   #:args (arg)
   (gen-migration arg)))
