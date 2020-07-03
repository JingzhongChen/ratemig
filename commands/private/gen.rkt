#lang racket/base

(require racket/cmdline
         racket/runtime-path
         "./util.rkt")

(provide gen-migration)

(define migrate-directory
  "./db/migrate")

(define (gen-migration description)
  (define current-seconds-number (current-from-year-to-second-number))
  (define a-file
    (string-append
     (number->string current-seconds-number)
     "-"
     description
     ".rkt"))
  (define abs-file (build-path migrate-directory a-file))
  (printf "gen migration file:  ~a\n" a-file)
  (call-with-output-file abs-file
    (lambda (out)
      (display "((up\n  )\n (down\n  ))" out))
    #:exists 'error)
  abs-file)

(module+ test
  (require rackunit
           rackunit/text-ui)

  (run-tests
   (test-suite
    "bin/gen test suite 1"

    (test-case
        "test gen-migration"
      (define abs-file (gen-migration "create-a-table"))
      (check-true (file-exists? abs-file))
      (begin
        (delete-file abs-file))))
   ))
