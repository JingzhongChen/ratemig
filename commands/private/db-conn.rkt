#lang racket/base

(require db/base
         db/mysql
         racket/runtime-path
         racket/function
         racket/list
         "./config/env.rkt"
         "db-config-structs.rkt"
         "./config.rkt")

(provide db-conn)

;; (define-runtime-path db-config-production-path
;;   "../../config/database-production.rkt")

;; (define-runtime-path db-config-development-path
;;   "../../config/database-development.rkt")

;; (define-runtime-path db-config-test-path
;;   "../../config/database-test.rkt")

(define database-config-path
  (case (identity environment)
    [("production")
     "./config/database-production.rkt"]
    [("development")
     "./config/database-development.rkt"]
    [("test")
     "./config/database-test.rkt"]
    [else
     "./config/database.rkt"]))

(define (read-db-config)
  (with-input-from-file database-config-path
    (lambda ()
      (read))))

(define db-conn
  (virtual-connection
   (connection-pool
    (lambda ()
      (make-connect)))))

(define (make-connect)
  (define db-config (read-db-config))
  (define db (get-conf* 'db db-config))
  (define server (get-conf 'server db))
  (define port (get-conf 'port db))
  (define database (get-conf 'database db))
  (define user (get-conf 'user db))
  (define password (get-conf 'password db))
  (mysql-connect #:server server
                 #:port port
                 #:database database
                 #:user user
                 #:password password))

(module+ test
  (require rackunit)
  (test-case
      "Check to parse db config file."
    (check-equal? (get-conf* 'db (read-db-config))
                  '((server "localhost")
                    (port 3306)
                    (database "racket-migrate-test-db")
                    (user "zl_test")
                    (password  "123456")))
    (check-equal? (get-conf 'server (get-conf* 'db (read-db-config)))
                  "localhost"))

  (test-case
      "Check connection."
    (define connect (make-connect))
    (check-pred connection? connect)
    (check-pred void? (disconnect connect)))

  (test-case
      "Check db-conn."
    (check-pred connection? db-conn)
    (check-pred (lambda (x) (not (connected? x))) db-conn)
    (query-value db-conn "select 1")
    (check-pred connected? db-conn)
    (disconnect db-conn)
    (check-pred (lambda (x) (not (connected? x))) db-conn)))
