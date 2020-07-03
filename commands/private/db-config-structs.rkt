#lang racket/base

(require racket/contract)

(struct db-config (db))
(struct db (server port database user password))

(provide/contract
 [struct db-config ([db db?])]
 [struct db ([server string?]
             [port integer?]
             [database string?]
             [user string?]
             [password string?])])
