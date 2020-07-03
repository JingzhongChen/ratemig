#lang racket/base

(require racket/cmdline
         racket/function
         racket/runtime-path
         racket/list
         racket/bool
         racket/string
         racket/sandbox
         compatibility/defmacro
         db/base
         "./config/env.rkt"
         "./db-conn.rkt"
         "./config.rkt"
         "./util.rkt")

(provide migrate rollback)

(define migrate-directory
  "./db/migrate")

(define (get-file-by-prefix files prefix)
  (define filter-list (filter (lambda (x) (string-prefix? x prefix)) files))
  (if (not-empty? filter-list)
      (car filter-list)
      #f))

(define (migrate-files)
  (filter (lambda (x)
            (not (false?
                  (regexp-match #px"^[2-9][0-9]{13}[-]" x))))
          (map path->string
               (directory-list migrate-directory))))

(define (get-schema-migrations)
  (query-list db-conn
              "SELECT id FROM `schema_migrations`"))

(define (check-duplicate-prefix file-prefixes)
  (define duplicate-prefix (check-duplicates file-prefixes))
  (when duplicate-prefix
    (raise
     (make-exn:fail:filesystem
      (string-append "duplicate prefix: " duplicate-prefix)
      (current-continuation-marks)))))

(define (get-not-run-prefixes)
  (define file-prefixes (map (lambda (x) (substring x 0 14)) (migrate-files)))
  (check-duplicate-prefix file-prefixes)
  (define schema-migrations (map number->string (get-schema-migrations)))
  (filter (lambda (x) (false? (member x schema-migrations))) file-prefixes))

(define (read-migrate-file a-file)
  (with-input-from-file (build-path migrate-directory a-file)
    (lambda ()
      (read))))

(define (get-sql-commands a-file up-or-down)
  (define migrate-cmds (read-migrate-file a-file))
  (get-conf* up-or-down migrate-cmds))

(define (get-up-commands a-file)
  (get-sql-commands a-file 'up))

(define (get-down-commands a-file)
  (get-sql-commands a-file 'down))

(define (update-version-file a-version)
  (define version-number
    (if (string? a-version)
        (string->number a-version)
        a-version))
  (call-with-output-file (build-path migrate-directory "version")
    (lambda (out)
      (write version-number out))
    #:exists 'truncate))

(define (update-version-table a-file mode)
  (define version-number
    (string->number (substring a-file 0 14)))
  (define mid-file-name
    (string-trim (substring a-file 15) ".rkt" #:left? #f #:right? #t))

  (cond [(equal? mode 'new)
         (query-exec db-conn
                     (string-append
                      "INSERT INTO `schema_migrations` (`id`, `applied`, `description`) VALUES "
                      (format
                       "(~a, '~a', '~a');"
                       version-number (current-milliseconds) mid-file-name)))]
        [(equal? mode 'rollback)
         (query-exec db-conn
                     (format
                      "DELETE FROM `schema_migrations` WHERE `schema_migrations`.`id` = ~a;"
                      version-number))]))

(define (base-top-eval)
  (make-evaluator
   '(begin)
   '(require compatibility/defmacro)
   '(require ratemig/parsers/parser)
   '(define-macro (eval-statement sql-expr)
      sql-expr)))

(define (db-exec-in-sandbox sql-expr)
  (define sql-strings ((base-top-eval) `(eval-statement ,sql-expr)))
  (if (list? sql-strings)
      (map (lambda (str)
             (query-exec db-conn str))
           sql-strings)
      (query-exec db-conn sql-strings)))

(define (run-each-migrate files)
  (define (migrate-proc a-file)
    (printf "~S\n" `(migrate-proc a-file ,a-file))
    (map (lambda (x) (db-exec-in-sandbox x))
         (get-up-commands a-file))
    (update-version-table a-file 'new)
    (update-version-file (substring a-file 0 14)))
  (map migrate-proc files))

(define (create-schema-migrations-table)
  (unless (table-exists? db-conn "schema_migrations")
    (query-exec db-conn
                (string-append
                 "CREATE TABLE `schema_migrations` "
                 "(`id` bigint(20) NOT NULL, "
                 "`applied` bigint(20) NOT NULL, "
                 "`description` varchar(1024) DEFAULT NULL)"))
    (query-exec db-conn
                (string-append
                 "ALTER TABLE `schema_migrations` "
                 "  ADD UNIQUE KEY `id` (`id`)"))))

(define (migrate)
  (displayln "begin migrate")
  (define files (migrate-files))
  (unless (table-exists? db-conn "schema_migrations")
    (create-schema-migrations-table))
  (define sort-prefixes (sort (get-not-run-prefixes) string<?))
  (define final-files (map (lambda (x) (get-file-by-prefix files x)) sort-prefixes))
  (run-each-migrate final-files)
  (displayln "migrate complete"))

(define (get-recent-migrated-version)
  (query-maybe-value
   db-conn
   "SELECT `id` FROM `schema_migrations` ORDER BY `schema_migrations`.`id` DESC LIMIT 1"))

(define (run-rollback recent-version)
  (define m-files (migrate-files))
  ;;check duplicate
  (define file-prefixes (map (lambda (x) (substring x 0 14)) m-files))
  (check-duplicate-prefix file-prefixes)

  (define a-file (get-file-by-prefix m-files (number->string recent-version)))
  (printf "rollback ~a\n" a-file)
  (define cmds (get-down-commands a-file))
  (map (lambda (x) (db-exec-in-sandbox x)) cmds)
  (update-version-table a-file 'rollback)

  (define b-recent-version (get-recent-migrated-version))
  (if b-recent-version
      (update-version-file b-recent-version)
      (update-version-file 0)))

(define (rollback)
  (displayln "begin rollback")
  (define recent-version (get-recent-migrated-version))
  (if recent-version
      (begin
        (run-rollback recent-version)
        (displayln "rollback complete"))
      (displayln "Done nothing, all migrations were rollbacked")))

(module+ test
  (require rackunit
           rackunit/text-ui
           racket/file)

  (run-tests
   (test-suite
    "bin/db test suite 1"
    #:before (lambda ()
               (unless (equal? environment "test")
                 (raise (make-exn:test
                         "This tests are only run in test environment."
                         (current-continuation-marks)))))

    (test-case
        "check permissions of S-expression of migration file."
      (sleep 1.2)
      (define current-seconds-number (current-from-year-to-second-number))
      (define a-file
        (string-append
         (number->string current-seconds-number)
         "-check-permissions-7682.rkt"))
      (define abs-file (build-path migrate-directory a-file))
      (define migrate-expr
        `((up
           (begin
             (delete-file (build-path
                           ,migrate-directory
                           "version"))
             "SELECT 1"))
          (down
           "SELECT 1")))
      (after
       (define out (open-output-file abs-file #:exists 'replace))
       (write migrate-expr out)
       (close-output-port out)

       (with-handlers ([exn:fail:filesystem?
                        (lambda (e)
                          ;; (printf "exn: ~a\n" e)
                          (void))])
         (call-with-exception-handler
          (lambda (e) (make-exn:fail:filesystem (exn-message e)
                                                (exn-continuation-marks e)))
          (lambda ()
            (migrate)))
         (check-true #f))

       (delete-file abs-file))

      (sleep 1.2)
      (define another-current-seconds-number
        (current-from-year-to-second-number))
      (define another-file
        (string-append
         (number->string another-current-seconds-number)
         "-check-permissions-7683.rkt"))
      (define another-abs-file (build-path migrate-directory another-file))

      (define another-migrate-expr
        `((up
           (begin
             (call-with-input-file
               (string->path "/tmp/ratemig-test/a-file-for-test.foo")
               (lambda (in)
                 (read in)))
             "SELECT 1"))
          (down
           "SELECT 1")))

      (make-directory* "/tmp/ratemig-test")

      (after
       (define out (open-output-file another-abs-file #:exists 'replace))
       (write another-migrate-expr out)
       (close-output-port out)

       (call-with-output-file
         "/tmp/ratemig-test/a-file-for-test.foo"
         (lambda (out)
           (write "test" out))
         #:exists 'replace)

       (with-handlers ([exn:fail:filesystem?
                        (lambda (e)
                          ;; (printf "exn: ~a\n" e)
                          (void))])
         (call-with-exception-handler
          (lambda (e) (make-exn:fail:filesystem (exn-message e)
                                                (exn-continuation-marks e)))
          (lambda ()
            (migrate)))
         (check-true #f))
       (delete-file another-abs-file)))

    (test-case
        "test migrate S-expression SQL stemment"
      (sleep 1.2)
      (define current-seconds-number (current-from-year-to-second-number))
      (define a-file
        (string-append
         (number->string current-seconds-number)
         "-create-some-tables-8953475.rkt"))
      (define abs-file (build-path migrate-directory a-file))
      (after
       (define out (open-output-file abs-file #:exists 'replace))
       (define up-sql
         (string-append
          "(create-table numbers_test_34390iudo ("
          "(n INT #:null #f)"
          "(t text)"
          ") #:id #f)"
          "(primary-key numbers_test_34390iudo n)"
          "\"INSERT INTO `numbers_test_34390iudo` set `n` = '1', `t` = 'numbers test'; \""
          "(add-column numbers_test_34390iudo name varchar(255) #:null #f #:default \"user\")"
          "(add-column numbers_test_34390iudo other_name varchar(255) #:null #f #:default \"user\")"
          "(rename-column numbers_test_34390iudo name new_name varchar(255))"
          "(change-column numbers_test_34390iudo new_name new_name varchar(125) #:null #f #:default \"user\")"
          "(index numbers_test_34390iudo new_name #:unique #t #:index-name index_new_name)"
          "(remove-column numbers_test_34390iudo other_name)"
          ))
       (define down-sql
          "(drop-table numbers_test_34390iudo)")
       (display (string-append "((up " up-sql  ") (down " down-sql  "))") out)
       (close-output-port out)
       (when (table-exists? db-conn "numbers_test_34390iudo")
         (query-exec db-conn "DROP TABLE `numbers_test_34390iudo`"))
       (check-false (table-exists? db-conn "numbers_test_34390iudo"))
       (migrate)
       (check-true (table-exists? db-conn "numbers_test_34390iudo"))
       (check-equal?
        (query-row db-conn
                   "SELECT `n`,`t`,`new_name` FROM `numbers_test_34390iudo` WHERE `n` = ?"
                   1)
        #(1 "numbers test" "user"))
       (rollback)
       (check-false (table-exists? db-conn "numbers_test_34390iudo"))
       (begin
         (delete-file abs-file))))

    (test-case
        "test S-expression SQL: commannd which return sql list string and run each string."
      (sleep 1.2)
      (define current-seconds-number (current-from-year-to-second-number))
      (define a-file
        (string-append
         (number->string current-seconds-number)
         "-create-some-tables-6823.rkt"))
      (define abs-file (build-path migrate-directory a-file))
      (after
       (define out (open-output-file abs-file #:exists 'replace))
       (define up-sql
         (string-append
          "(create-table numbers_test_863 ("
          "(n INT #:null #f)"
          "(t text)"
          "))"
          "\"INSERT INTO `numbers_test_863` set `id` = '2', `n` = '1', `t` = 'numbers test'; \""))
       (define down-sql
         "(drop-table numbers_test_863)")
       (display (string-append "((up " up-sql  ") (down " down-sql  "))") out)
       (close-output-port out)
       (when (table-exists? db-conn "numbers_test_863")
         (query-exec db-conn "DROP TABLE `numbers_test_863`"))
       (check-false (table-exists? db-conn "numbers_test_863"))
       (migrate)
       (check-true (table-exists? db-conn "numbers_test_863"))
       (check-equal?
        (query-row db-conn
                   "SELECT `id`, `n`,`t` FROM `numbers_test_863` WHERE `n` = ?"
                   1)
        #(2 1 "numbers test"))
       (rollback)
       (check-false (table-exists? db-conn "numbers_test_863"))
       (begin
         (delete-file abs-file))))

    (test-case
        "test migrate S-expression SQL stemment: rename-table"
      (sleep 1.2)
      (define current-seconds-number (current-from-year-to-second-number))
      (define a-file
        (string-append
         (number->string current-seconds-number)
         "-create-some-tables-9768.rkt"))
      (define abs-file (build-path migrate-directory a-file))
      (after
       (define out (open-output-file abs-file #:exists 'replace))
       (define up-sql
         (string-append
          "(create-table numbers_test_67453 ("
          "(n INT #:null #f)"
          "(t text)"
          "))"
          "(rename-table numbers_test_67453 numbers_test_893)"
          ))
       (define down-sql
         "(drop-table numbers_test_893)")
       (display (string-append "((up " up-sql  ") (down " down-sql  "))") out)
       (close-output-port out)
       (when (table-exists? db-conn "numbers_test_67453")
         (query-exec db-conn "DROP TABLE `numbers_test_67453`"))
       (when (table-exists? db-conn "numbers_test_893")
         (query-exec db-conn "DROP TABLE `numbers_test_893`"))
       (check-false (table-exists? db-conn "numbers_test_67453"))
       (check-false (table-exists? db-conn "numbers_test_893"))
       (migrate)
       (check-false (table-exists? db-conn "numbers_test_67453"))
       (check-true (table-exists? db-conn "numbers_test_893"))
       (rollback)
       (check-false (table-exists? db-conn "numbers_test_893"))
       (begin
         (delete-file abs-file))))

    (test-case
        "test rollback"
      (sleep 1.2)
      (define current-seconds-number (current-from-year-to-second-number))
      (define a-file
        (string-append
         (number->string current-seconds-number)
         "-create-some-tables.rkt"))
      (define abs-file (build-path migrate-directory a-file))
      (after
       (define out (open-output-file abs-file #:exists 'replace))
       (define up-sql
         (string-append
          "\"CREATE TABLE `migrate-test-table-2342` "
          "(`id` bigint(20) NOT NULL, "
          "`description` varchar(1024) DEFAULT NULL)\""))
       (define down-sql
         (string-append
          "\"DROP TABLE `migrate-test-table-2342`\""))
       (display (string-append "((up " up-sql  ") (down " down-sql  "))") out)
       (close-output-port out)
       (when (table-exists? db-conn "migrate-test-table-2342")
         (query-exec db-conn "DROP TABLE `migrate-test-table-2342`"))
       (check-false (table-exists? db-conn "migrate-test-table-2342"))
       (migrate)
       (check-true (table-exists? db-conn "migrate-test-table-2342"))
       (check-equal? (get-recent-migrated-version) current-seconds-number)
       (check-equal? current-seconds-number
        (call-with-input-file (build-path migrate-directory "version")
         (lambda (in)
           (read in))))
       (rollback)
       (check-false (table-exists? db-conn "migrate-test-table-2342"))
       (check-not-equal? (get-recent-migrated-version) current-seconds-number)
       (check-not-equal? current-seconds-number
                     (call-with-input-file (build-path migrate-directory "version")
                       (lambda (in)
                         (read in))))
       (begin
         (delete-file abs-file))))

    (test-case
        "test get-recent-migrated-versions"
      (sleep 1.2)
      (define current-seconds-number (current-from-year-to-second-number))
      (define a-file
        (string-append
         (number->string current-seconds-number)
         "-create-some-tables.rkt"))
      (create-schema-migrations-table)
      (update-version-table a-file 'new)
      (check-equal? (get-recent-migrated-version) current-seconds-number)
      (update-version-table a-file 'rollback))

    (test-case
        "test update-version-table"
      (sleep 1.2)
      (define current-seconds-number (current-from-year-to-second-number))
      (define a-file
        (string-append
         (number->string current-seconds-number)
         "-create-some-tables.rkt"))
      (create-schema-migrations-table)
      (update-version-table a-file 'new)
      (check-equal? (query-value db-conn
                                 "SELECT `description` FROM `schema_migrations` WHERE `id` = ?"
                                 current-seconds-number)
                    "create-some-tables")
      (update-version-table a-file 'rollback)
      (check-equal? (query-maybe-value db-conn
                                 "SELECT `description` FROM `schema_migrations` WHERE `id` = ?"
                                 current-seconds-number)
                    #f))

    (test-case
        "test update-version-file"
      (define version-path (build-path migrate-directory "version"))
      (define old-version
        (if (eof-object?
             (call-with-input-file version-path
               (lambda (in)
                 (read in))))
            0
            (call-with-input-file version-path
              (lambda (in)
                (read in)))))
      (after
       (update-version-file 20170101010102)
       (check-equal? 20170101010102
                     (call-with-input-file version-path
                       (lambda (in)
                         (read in))))
       (update-version-file "20170101010103")
       (check-equal? 20170101010103
                     (call-with-input-file version-path
                       (lambda (in)
                         (read in))))
       (call-with-output-file version-path
         (lambda (out)
                (write old-version out))
         #:exists 'truncate)))

    (test-case
        "test get-up-commands"
      (define a-file "20171215102353-create-some-tables.rkt")
      (define abs-file (build-path migrate-directory a-file))
      (after
       (define out (open-output-file abs-file #:exists 'replace))
       (define up-sql
         (string-append
          "\"CREATE TABLE `migrate-test-table-82ehi7o92eckcc` "
          "(`id` bigint(20) NOT NULL, "
          "`description` varchar(1024) DEFAULT NULL)\""))
       (display (string-append "((up " up-sql  ") (down))") out)
       (close-output-port out)
       (define up-sql-normal-str
         (string-append
          "CREATE TABLE `migrate-test-table-82ehi7o92eckcc` "
          "(`id` bigint(20) NOT NULL, "
          "`description` varchar(1024) DEFAULT NULL)"))
       (define commands (get-up-commands a-file))
       (check-equal? (car commands) up-sql-normal-str)
       (delete-file abs-file)))

    (test-case
        "test run-each-migrate"
      (define a-file "20171215102325-create-some-tables.rkt")
      (define abs-file (build-path migrate-directory a-file))
      (after
       (define out (open-output-file abs-file #:exists 'replace))
       (define up-sql
         (string-append
          "\"CREATE TABLE `migrate-test-table-82ehi7o92eckab` "
          "(`id` bigint(20) NOT NULL, "
          "`description` varchar(1024) DEFAULT NULL)\""))
       (display (string-append "((up " up-sql  ") (down))") out)
       (close-output-port out)
       (when (table-exists? db-conn "migrate-test-table-82ehi7o92eckab")
         (query-exec db-conn "DROP TABLE `migrate-test-table-82ehi7o92eckab`"))
       (run-each-migrate `(,a-file))
       (check-true (table-exists? db-conn "migrate-test-table-82ehi7o92eckab"))
       (begin
         (delete-file abs-file)
         (query-exec db-conn "DROP TABLE `migrate-test-table-82ehi7o92eckab`"))))

    (test-case
        "test migrate"
      (define a-file "20171215102328-create-some-tables.rkt")
      (define abs-file (build-path migrate-directory a-file))
      (after
       (define out (open-output-file abs-file #:exists 'replace))
       (define up-sql
         (string-append
          "\"CREATE TABLE `migrate-test-table-82ehi7o92ebhed` "
          "(`id` bigint(20) NOT NULL, "
          "`description` varchar(1024) DEFAULT NULL)\""))
       (display (string-append "((up " up-sql  ") (down))") out)
       (close-output-port out)
       (when (table-exists? db-conn "migrate-test-table-82ehi7o92ebhed")
         (query-exec db-conn "DROP TABLE `migrate-test-table-82ehi7o92ebhed`"))
       (migrate)
       (check-true (table-exists? db-conn "migrate-test-table-82ehi7o92ebhed"))
       (begin
         (delete-file abs-file)
         (query-exec db-conn "DROP TABLE `migrate-test-table-82ehi7o92ebhed`"))))

    (test-case
        "test get-file-by-prefix"
      (check-equal?
       "20171105212515-create-some-tables.rkt"
       (get-file-by-prefix
        '("20171105212515-create-some-tables.rkt"
          "20171105212516-create-other-tables.rkt")
        "20171105212515")))

    (test-case
        "test migrate-files"
      (define file1 (build-path migrate-directory "20171105212515-create-some-tables.rkt"))
      (define other-file (build-path migrate-directory "20171105212516-create-other-tables.rkt"))

      (after
       (define out (open-output-file file1 #:exists 'replace))
       (close-output-port out)
       (define other-out (open-output-file other-file #:exists 'replace))
       (close-output-port other-out)

       (define files (migrate-files))
       (check-pred
        (lambda (x)
          (not (false?
                (member "20171105212515-create-some-tables.rkt" x))))
        files)
       (check-pred
        (lambda (x)
          (not (false?
                (member "20171105212516-create-other-tables.rkt" x))))
        files)
       (begin
         (delete-file file1)
         (delete-file other-file))))))

  (run-tests
   (test-suite
    "bin/db test suite"
    #:before (lambda ()
               (unless (equal? environment "test")
                 (raise (make-exn:test
                         "This tests are only run in test environment."
                         (current-continuation-marks))))

               (unless (table-exists? db-conn "schema_migrations")
                 (query-exec db-conn
                             (string-append
                              "CREATE TABLE `schema_migrations` "
                              "(`id` bigint(20) NOT NULL, "
                              "`applied` timestamp NOT NULL DEFAULT "
                              "CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP, "
                              "`description` varchar(1024) DEFAULT NULL)"))
                 (query-exec db-conn
                             (string-append
                              "ALTER TABLE `schema_migrations` "
                              "  ADD UNIQUE KEY `id` (`id`)")))
               (query-exec db-conn
                           "TRUNCATE `schema_migrations`"))
    #:after (lambda ()
              (query-exec db-conn
                          "TRUNCATE `schema_migrations`")
              (query-exec db-conn
                          "DROP TABLE `schema_migrations`"))

    (test-case
        "test get-schema-migrations."
      (query-exec db-conn
                  (string-append
                   "INSERT INTO `schema_migrations` (`id`, `applied`, `description`) VALUES "
                   "(20171105212515, '2017-11-05 00:35:53', 'init-schema'),"
                   "(20171105215026, '2017-11-05 00:35:53', 'create-oauth-tables'),"
                   "(20171108140349, '2017-11-08 00:58:39', 'add-user-id-to-clients'),"
                   "(20171115210059, '2017-11-15 00:46:52', 'create-commands');"))
      (check-equal? (get-schema-migrations)
                    '(20171105212515
                      20171105215026
                      20171108140349
                      20171115210059)))

    (test-case
        "test duplicate-prefix."
      (define file1 (build-path migrate-directory "20171105212515-create-some-tables.rkt"))
      (define other-file (build-path migrate-directory "20171105212515-create-other-tables.rkt"))

      (after
       (define out (open-output-file file1 #:exists 'replace))
       (close-output-port out)
       (define other-out (open-output-file other-file #:exists 'replace))
       (close-output-port other-out)

       (with-handlers ([exn:fail:filesystem?
                        (lambda (e)
                          ;; return true when don't raise duplicate error.
                          (check-true #t))])
         (get-not-run-prefixes)
         ;; return failure when don't raise duplicate error.
         (check-true #f))

       (begin
         (delete-file file1)
         (delete-file other-file))))

    (test-case
        "test get-not-run-prefixes."
      (define file1 (build-path migrate-directory "20171105210810-create-some-tables.rkt"))
      (define other-file (build-path migrate-directory "20171105210811-create-other-tables.rkt"))
      (after
       (query-exec db-conn
                   (string-append
                    "INSERT INTO `schema_migrations` (`id`, `applied`, `description`) VALUES "
                    "(20171105210810, '2017-11-05 00:35:53', 'init-schema');"))

       (define out (open-output-file file1 #:exists 'replace))
       (close-output-port out)

       (define other-out (open-output-file other-file #:exists 'replace))
       (close-output-port other-out)

       (check-pred false? (member "20171105210810" (get-not-run-prefixes)))
       (check-pred (lambda (x) (not (empty? x)))
                   (member "20171105210811" (get-not-run-prefixes)))

       (begin
         (delete-file file1)
         (delete-file other-file)))))))
