#lang racket/base

(require racket/string)
(require (for-syntax racket/base
                     racket/string))

(provide primary-key
         index
         drop-table
         create-table
         rename-table
         change-column
         rename-column
         add-column
         remove-column)

(define-syntax (primary-key stx)
  (syntax-case stx ()
    [(_ table-name key)
     #`(string-append "ALTER TABLE `"
                      (symbol->string 'table-name)
                      "` "
                      "ADD PRIMARY KEY (`"
                      (symbol->string 'key)
                      "`);")]
    [else
     #`(raise
        (raise-syntax-error #f "mismatch params!" #f #'#,stx)
        )]))

(define (gen-index-name table-name key)
  (define key-string
    (if (list? key)
        (string-join
         (map symbol->string key)
         "_")
        (symbol->string key)))
  (string-append "index_"
                 (symbol->string table-name)
                 "_on_"
                 key-string))

(define (gen-index-key-list key)
  (if (list? key)
      (string-join
       (map (lambda (x) (string-append "`" x "`"))
            (map symbol->string key))
       ",")
      (string-append "`" (symbol->string key) "`")))

(define-syntax (index stx)
  (syntax-case stx ()
    [(_ table-name key #:unique unique? #:index-name index-name)
     #`(string-append "ALTER TABLE `"
                      (symbol->string 'table-name)
                      "` ADD"
                      (if (equal? 'unique? '#f) "" " UNIQUE")
                      " KEY `"
                      (if (equal? 'index-name '#f)
                          (gen-index-name 'table-name 'key)
                          (symbol->string 'index-name))
                      "` ("
                      (gen-index-key-list 'key)
                      ");"
                      )]
    [(_ table-name key #:index-name index-name #:unique unique?)
     #'(index table-name key #:unique unique? #:index-name index-name)]
    [(_ table-name key #:index-name index-name)
     #'(index table-name key #:unique #f #:index-name index-name)]
    [(_ table-name key #:unique unique?)
     #'(index table-name key #:unique unique? #:index-name #f)]
    [(_ table-name key)
     #'(index table-name key #:unique #f #:index-name #f)]
    [else
     #`(raise
        (raise-syntax-error #f "mismatch params!" #f #'#,stx))]))

(define-syntax (drop-table stx)
  (syntax-case stx ()
    [(_ table)
     #'(string-append "DROP TABLE "
                      (symbol->string 'table)
                      ";")]))

(define-syntax (create-table stx)
  (syntax-case stx ()
    [(_ table-name ((column-name ...) ...) #:id id? #:timestamp timestamp?)
     #'(let* ([id-string (if (equal? 'id? '#t)
                             (list "`id` int(11) NOT NULL")
                             '())]
              [id-modify (if (equal? 'id? '#t)
                             (list
                              (string-append "ALTER TABLE `"
                                             (symbol->string 'table-name)
                                             "` "
                                             "ADD PRIMARY KEY (`id`);")
                              (string-append "ALTER TABLE `"
                                             (symbol->string 'table-name)
                                             "` "
                                             "MODIFY `id` int(11) NOT NULL AUTO_INCREMENT;"))
                             '())]
              [timestamp-string (if (equal? 'timestamp? '#t)
                                    (list
                                     "`created_at` datetime NOT NULL \
DEFAULT CURRENT_TIMESTAMP"
                                     "`updated_at` datetime NOT NULL \
DEFAULT CURRENT_TIMESTAMP")
                                    '())]
              [init-column-list (list
                                 (parse-column column-name ...)
                                 ...)]
              [column-list (append id-string init-column-list timestamp-string)]
              [table-string (string-append "CREATE TABLE `"
                                           (symbol->string 'table-name)
                                           "` ("
                                           (string-join
                                            column-list
                                            ",")
                                           ") ENGINE=InnoDB DEFAULT CHARSET=utf8;")])
         (cons table-string id-modify))]
    [(_ table-name ((column-name ...) ...) #:timestamp timestamp? #:id id?)
     #'(create-table table-name ((column-name ...) ...) #:id id? #:timestamp timestamp?)]
    [(_ table-name ((column-name ...) ...) #:id id?)
     #'(create-table table-name ((column-name ...) ...) #:id id? #:timestamp #t)]
    [(_ table-name ((column-name ...) ...) #:timestamp timestamp?)
     #'(create-table table-name ((column-name ...) ...) #:id #t #:timestamp timestamp?)]
    [(_ table-name ((column-name ...) ...))
     #'(create-table table-name ((column-name ...) ...) #:id #t #:timestamp #t)]))

(define (default-string default)
  (cond [(string? default) (string-append "'" default "'")]
        [(number? default) (string-append "'" (number->string default) "'")]
        [(symbol? default)
         (cond [(equal? default 'null) "NULL"]
               [else
                (symbol->string default)])]
        [else (raise
               (raise-syntax-error #f "default type error!" #f default)
               )]))

(define-syntax (parse-column stx)
  #'(printf "~v\n" stx)
  (syntax-case stx ()
    [(_ column-name type (len) #:null is-null? #:default default)
     #'(string-append (parse-column column-name type (len) #:null is-null?)
                      (string-append " DEFAULT " (default-string 'default)))]

    [(_ column-name type #:null is-null? #:default default)
     #'(parse-column column-name type (#f) #:null is-null? #:default default)]

    [(_ column-name type (len) #:default default #:null is-null?)
     #'(parse-column column-name type (len) #:null is-null? #:default default)]

    [(_ column-name type #:default default #:null is-null?)
     #'(parse-column column-name type (#f) #:null is-null? #:default default)]

    [(_ column-name type (len) #:default default)
     #'(parse-column column-name type (len) #:null #t #:default default)]

    [(_ column-name type #:default default)
     #'(parse-column column-name type (#f) #:null #t #:default default)]

    [(_ column-name type (len) #:null is-null?)
     #'(string-append "`"
                      (symbol->string 'column-name)
                      "` "
                      (symbol->string 'type)
                      (if (equal? 'len '#f)
                          ""
                          (string-append "("
                                         (number->string 'len)
                                         ")"))
                      (if (equal? 'is-null? '#t)
                          ""
                          " NOT NULL"))]

    [(_ column-name type #:null is-null?)
     #'(parse-column column-name type (#f) #:null is-null?)]

    [(_ column-name type (len))
     #'(parse-column column-name type (len) #:null #t)]

    [(_ column-name type)
     #'(parse-column column-name type (#f) #:null #t)]))

(define-syntax (change-column stx)
  (syntax-case stx ()
    [(_ table-name column-name new-column-name)
     #'(string-append "ALTER TABLE `"
                      (symbol->string 'table-name)
                      "` CHANGE `"
                      (symbol->string 'column-name)
                      "` `"
                      (symbol->string 'new-column-name)
                      "`;")]
    [(_ table-name column-name)
     #`(raise
        (raise-syntax-error #f "mismatch params!" #f #'#,stx))]
    [(_ table-name column-name new-column-name ...)
     #'(string-append (string-append "ALTER TABLE `"
                                     (symbol->string 'table-name)
                                     "` CHANGE `"
                                     (symbol->string 'column-name)
                                     "` ")
                      (parse-column new-column-name ...)
                      ";")]
    [else
     #`(raise
        (raise-syntax-error #f "mismatch params!" #f #'#,stx))]))

;; ALTER TABLE "table_name" RENAME COLUMN "column 1" TO "column 2";
(define-syntax (rename-column stx)
  (syntax-case stx ()
    [(_ table-name column-name new-column-name data-type len)
     #'(change-column table-name column-name new-column-name data-type len)]
    [(_ table-name column-name new-column-name data-type)
     #'(change-column table-name column-name new-column-name data-type)]
    [else
     #`(raise
        (raise-syntax-error #f "mismatch params!" #f #'#,stx))]))

(define-syntax (add-column stx)
  (syntax-case stx ()
    [(_ table-name column-name)
     #`(raise
        (raise-syntax-error #f "mismatch params!" #f #'#,stx))]
    [(_ table-name column-name ...)
     #'(string-append (string-append "ALTER TABLE `"
                                     (symbol->string 'table-name)
                                     "` ADD ")
                      (parse-column column-name ...)
                      ";")]
    [else
     #`(raise
        (raise-syntax-error #f "mismatch params!" #f #'#,stx))]))

(define-syntax (remove-column stx)
  (syntax-case stx ()
    [(_ table-name column-name)
     #'(string-append "ALTER TABLE `"
                      (symbol->string 'table-name)
                      "` DROP `"
                      (symbol->string 'column-name)
                      "`;")]
    [else
     #`(raise
        (raise-syntax-error #f "mismatch params!" #f #'#,stx))]))

(define-syntax (rename-table stx)
  (syntax-case stx ()
    [(_ table-name new-table-name)
     #'(string-append "ALTER TABLE `"
                      (symbol->string 'table-name)
                      "` RENAME `"
                      (symbol->string 'new-table-name)
                      "`;")]
    [else
     #`(raise
        (raise-syntax-error #f "mismatch params!" #f #'#,stx))]))

(module+ test
  (require rackunit)

  (test-case
      "test primary-key"
    (check-equal? (primary-key test_tb id)
                  (string-append "ALTER TABLE `test_tb` "
                                 "ADD PRIMARY KEY (`id`);"))
    (check-equal? (primary-key test_tb_other test_tb_other)
                  (string-append "ALTER TABLE `test_tb_other` "
                                 "ADD PRIMARY KEY (`test_tb_other`);"))
    (check-not-equal? (primary-key test_tb_other test_tb_other_ex)
                      (string-append "ALTER TABLE `test_tb_other` "
                                     "ADD PRIMARY KEY (`test_tb_other`);"))

    (with-handlers ([exn:fail:syntax?
                     (lambda (e)
                       (check-true #t)
                       )])
      (primary-key test_tb_other test_tb_other_ex other-sxpr)
      (check-true #f)))

  (test-case
      "test index"
    (check-equal? (index test_tb id)
                  (string-append "ALTER TABLE `test_tb` "
                                 "ADD KEY `index_test_tb_on_id` (`id`);"))
    (check-equal? (index test_tb_other test_tb_col)
                  (string-append "ALTER TABLE `test_tb_other` "
                                 "ADD KEY `index_test_tb_other_on_test_tb_col` "
                                 "(`test_tb_col`);"))
    (check-not-equal? (index test_tb_other test_tb_col_ex)
                      (string-append "ALTER TABLE `test_tb_other` "
                                     "ADD KEY `index_test_tb_on_test_tb_col` "
                                     "(`test_tb_col`);"))
    (check-equal? (index test_tb id #:unique #t)
                  (string-append "ALTER TABLE `test_tb` "
                                 "ADD UNIQUE KEY `index_test_tb_on_id` (`id`);"))
    (check-equal?
     (index test_tb_other col_2 #:index-name index_other_col_2)
     (string-append "ALTER TABLE `test_tb_other` "
                    "ADD KEY `index_other_col_2` (`col_2`);"))
    (check-equal?
     (index test_tb_other test_col #:unique #t #:index-name index_other_test_col)
     (string-append "ALTER TABLE `test_tb_other` "
                    "ADD UNIQUE KEY `index_other_test_col` (`test_col`);"))
    (check-equal?
     (index test_tb_other test_col #:index-name index_other_test_col #:unique #t)
     (string-append "ALTER TABLE `test_tb_other` "
                    "ADD UNIQUE KEY `index_other_test_col` (`test_col`);"))
    (check-equal?
     (index test_tb_other
            (test_col test_desc)
            #:index-name index_other_test_col
            #:unique #t)
     (string-append "ALTER TABLE `test_tb_other` "
                    "ADD UNIQUE KEY `index_other_test_col` (`test_col`,`test_desc`);"))
    (check-equal?
     (index test_tb_other (test_col test_desc) #:unique #t)
     (string-append "ALTER TABLE `test_tb_other` "
                    "ADD UNIQUE KEY `index_test_tb_other_on_test_col_test_desc` "
                    "(`test_col`,`test_desc`);"))

    (with-handlers ([exn:fail:syntax?
                     (lambda (e)
                       (check-true #t)
                       )])
      (index test_tb_other test_tb_other_ex #:unique #f #:index-name other-sxpr other-exp)
      (check-true #f))
    )

  (test-case
      "test drop-table"
    (check-equal? (drop-table staffs) "DROP TABLE staffs;"))

  (test-case
      "test create-table"
    (check-equal? (create-table peers
                                ((user_id int(11) #:null #f)
                                 (user_name int(11) #:default "user" #:null #f)
                                 (mark varchar(255) #:null #f)
                                 (last_action_at datetime #:null #f)
                                 (last_ping_at datetime #:default NULL)
                                 (login_count int(11) #:default 0)))
                  (list
                   (string-append "CREATE TABLE `peers` ("
                                  "`id` int(11) NOT NULL,"
                                  "`user_id` int(11) NOT NULL,"
                                  "`user_name` int(11) NOT NULL DEFAULT 'user',"
                                  "`mark` varchar(255) NOT NULL,"
                                  "`last_action_at` datetime NOT NULL,"
                                  "`last_ping_at` datetime DEFAULT NULL,"
                                  "`login_count` int(11) DEFAULT '0',"
                                  "`created_at` datetime NOT NULL "
                                  "DEFAULT CURRENT_TIMESTAMP,"
                                  "`updated_at` datetime NOT NULL "
                                  "DEFAULT CURRENT_TIMESTAMP"
                                  ") ENGINE=InnoDB DEFAULT CHARSET=utf8;")
                   (string-append "ALTER TABLE `peers` "
                                  "ADD PRIMARY KEY (`id`);")
                   (string-append "ALTER TABLE `peers` MODIFY "
                                  "`id` int(11) NOT NULL AUTO_INCREMENT;"))))

  (test-case
      "test create-table #:id #f"
    (check-equal? (create-table peers
                                ((user_id int(11) #:null #f)
                                 (user_name int(11) #:default "user" #:null #f)
                                 (mark varchar(255) #:null #f)
                                 (last_action_at datetime #:null #f)
                                 (last_ping_at datetime #:default NULL)
                                 (login_count int(11) #:default 0))
                                #:id #f)
                  (list (string-append "CREATE TABLE `peers` ("
                                       "`user_id` int(11) NOT NULL,"
                                       "`user_name` int(11) NOT NULL DEFAULT 'user',"
                                       "`mark` varchar(255) NOT NULL,"
                                       "`last_action_at` datetime NOT NULL,"
                                       "`last_ping_at` datetime DEFAULT NULL,"
                                       "`login_count` int(11) DEFAULT '0',"
                                       "`created_at` datetime NOT NULL "
                                       "DEFAULT CURRENT_TIMESTAMP,"
                                       "`updated_at` datetime NOT NULL "
                                       "DEFAULT CURRENT_TIMESTAMP"
                                       ") ENGINE=InnoDB DEFAULT CHARSET=utf8;"))))

  (test-case
      "test create-table #:id #f, #:timestamp #f "
    (check-equal? (create-table peers
                                ((user_id int(11) #:null #f)
                                 (user_name int(11) #:default "user" #:null #f)
                                 (mark varchar(255) #:null #f)
                                 (last_action_at datetime #:null #f)
                                 (last_ping_at datetime #:default NULL)
                                 (login_count int(11) #:default 0))
                                #:id #f #:timestamp #f)
                  (list (string-append "CREATE TABLE `peers` ("
                                       "`user_id` int(11) NOT NULL,"
                                       "`user_name` int(11) NOT NULL DEFAULT 'user',"
                                       "`mark` varchar(255) NOT NULL,"
                                       "`last_action_at` datetime NOT NULL,"
                                       "`last_ping_at` datetime DEFAULT NULL,"
                                       "`login_count` int(11) DEFAULT '0'"
                                       ") ENGINE=InnoDB DEFAULT CHARSET=utf8;"))))

  (test-case
      "test create-table #:timestamp #f, #:id #f "
    (check-equal? (create-table peers
                                ((user_id int(11) #:null #f)
                                 (user_name int(11) #:default "user" #:null #f)
                                 (mark varchar(255) #:null #f)
                                 (last_action_at datetime #:null #f)
                                 (last_ping_at datetime #:default NULL)
                                 (login_count int(11) #:default 0))
                                #:timestamp #f #:id #f)
                  (list (string-append "CREATE TABLE `peers` ("
                                       "`user_id` int(11) NOT NULL,"
                                       "`user_name` int(11) NOT NULL DEFAULT 'user',"
                                       "`mark` varchar(255) NOT NULL,"
                                       "`last_action_at` datetime NOT NULL,"
                                       "`last_ping_at` datetime DEFAULT NULL,"
                                       "`login_count` int(11) DEFAULT '0'"
                                       ") ENGINE=InnoDB DEFAULT CHARSET=utf8;"))))

  (test-case
      "test create-table #:timestamp #f "
    (check-equal? (create-table peers
                                ((user_id int(11) #:null #f)
                                 (user_name int(11) #:default "user" #:null #f)
                                 (mark varchar(255) #:null #f)
                                 (last_action_at datetime #:null #f)
                                 (last_ping_at datetime #:default NULL)
                                 (login_count int(11) #:default 0))
                                #:timestamp #f)
                  (list (string-append "CREATE TABLE `peers` ("
                                       "`id` int(11) NOT NULL,"
                                       "`user_id` int(11) NOT NULL,"
                                       "`user_name` int(11) NOT NULL DEFAULT 'user',"
                                       "`mark` varchar(255) NOT NULL,"
                                       "`last_action_at` datetime NOT NULL,"
                                       "`last_ping_at` datetime DEFAULT NULL,"
                                       "`login_count` int(11) DEFAULT '0'"
                                       ") ENGINE=InnoDB DEFAULT CHARSET=utf8;")
                        (string-append "ALTER TABLE `peers` "
                                       "ADD PRIMARY KEY (`id`);")
                        (string-append "ALTER TABLE `peers` MODIFY "
                                       "`id` int(11) NOT NULL AUTO_INCREMENT;"))))

  (test-case
      "test change-column"
    (check-equal?
     (change-column peers user_name user_name varchar(255) #:default "user" #:null #f)
     (string-append "ALTER TABLE `peers` CHANGE `user_name` "
                    "`user_name` varchar(255) NOT NULL DEFAULT 'user';"))
    (check-equal?
     (change-column peers user_name user_name varchar(255) #:null #f #:default "user")
     (string-append "ALTER TABLE `peers` CHANGE `user_name` "
                    "`user_name` varchar(255) NOT NULL DEFAULT 'user';"))
    (check-equal?
     (change-column peers login_count new_login_count int(11) #:default 0)
     (string-append "ALTER TABLE `peers` CHANGE `login_count` "
                    "`new_login_count` int(11) DEFAULT '0';"))
    (check-equal?
     (change-column peers mark mark varchar(255) #:null #f)
     (string-append "ALTER TABLE `peers` CHANGE `mark` "
                    "`mark` varchar(255) NOT NULL;"))
    (check-equal?
     (change-column peers mark new_mark)
     (string-append "ALTER TABLE `peers` CHANGE `mark` "
                    "`new_mark`;"))

    (with-handlers ([exn:fail:syntax?
                     (lambda (e)
                       (check-true #t)
                       )])
      (change-column table-name column-name)
      (check-true #f)))

  (test-case
      "test rename-column"
    (check-equal? (rename-column peers login_name new_login_name varchar(255))
                  (string-append "ALTER TABLE `peers` CHANGE `login_name` "
                                 "`new_login_name` varchar(255);"))
    (with-handlers ([exn:fail:syntax?
                     (lambda (e)
                       (check-true #t)
                       )])
      (rename-column table_name column_name)
      (check-true #f))
    (with-handlers ([exn:fail:syntax?
                     (lambda (e)
                       (check-true #t)
                       )])
      (rename-column table_name column_name new_column_name)
      (check-true #f))
    (with-handlers ([exn:fail:syntax?
                     (lambda (e)
                       (check-true #t)
                       )])
      (rename-column table_name column_name new_name varchar(255) other_opt)
      (check-true #f)))

  (test-case
      "test add-column"
    (check-equal?
     (add-column peers user_name varchar(255) #:default "user" #:null #f)
     (string-append "ALTER TABLE `peers` ADD "
                    "`user_name` varchar(255) NOT NULL DEFAULT 'user';"))
    (check-equal?
     (add-column peers user_name varchar(255) #:null #f #:default "user")
     (string-append "ALTER TABLE `peers` ADD "
                    "`user_name` varchar(255) NOT NULL DEFAULT 'user';"))
    (check-equal?
     (add-column peers login_count int(11) #:default 0)
     (string-append "ALTER TABLE `peers` ADD "
                    "`login_count` int(11) DEFAULT '0';"))
    (check-equal?
     (add-column peers mark varchar(255) #:null #f)
     (string-append "ALTER TABLE `peers` ADD "
                    "`mark` varchar(255) NOT NULL;"))
    (check-not-equal?
     (add-column peers mark varchar(255) #:null #f)
     (string-append "ALTER TABLE `peers` ADD "
                    "`mark` varchar(255);"))

    (with-handlers ([exn:fail:syntax?
                     (lambda (e)
                       (check-true #t)
                       )])
      (change-column table-name column-name)
      (check-true #f)))

  (test-case
      "test remove-column"
    (check-equal?
     (remove-column peers mark)
     "ALTER TABLE `peers` DROP `mark`;")
    (check-not-equal?
     (remove-column peers mark)
     "ALTER TABLE `peers` ADD `mark`;")

    (with-handlers ([exn:fail:syntax?
                     (lambda (e)
                       (check-true #t)
                       )])
      (remove-column table-name)
      (check-true #f))
    (with-handlers ([exn:fail:syntax?
                     (lambda (e)
                       (check-true #t)
                       )])
      (remove-column table-name column-name other-opt)
      (check-true #f)))

  (test-case
      "test rename-table"
    (check-equal? (rename-table peers new_peers)
                  "ALTER TABLE `peers` RENAME `new_peers`;")
    (with-handlers ([exn:fail:syntax?
                     (lambda (e)
                       (check-true #t)
                       )])
      (rename-table peers new_peers other-opt)
      (check-true #f))
    (with-handlers ([exn:fail:syntax?
                     (lambda (e)
                       (check-true #t)
                       )])
      (rename-table table-name)
      (check-true #f)))
  )
