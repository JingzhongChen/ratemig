#lang info

(define collection "ratemig")
(define deps '("base" "sql"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/ratemig.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(jared))

(define compile-omit-paths '("commands/private/config/"
                             "commands/private/db/migrate/"))

(define test-omit-paths '("commands/private/config/"
                          "commands/private/db/migrate/"
                          "commands/gen.rkt"
                          "commands/migrate.rkt"
                          "commands/rollback.rkt"
                          "./main.rkt"
                          "./info.rkt"))
