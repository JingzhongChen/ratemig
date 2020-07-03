#lang racket/base

(provide environment)

;; "development" "production" "test"
(define env "test")

(define racket-env (getenv "RACKET_ENV"))

(define environment
  (if racket-env
      racket-env
      env))
