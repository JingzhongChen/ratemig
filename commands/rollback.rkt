#lang racket/base

(require racket/cmdline
         ratemig/commands/private/db)

(define main
  (command-line
   #:args
   ()
   (rollback)))
