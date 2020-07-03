#lang racket/base

(require racket/list)

(provide fetch-config get-conf get-conf*)

(define (fetch-config key confs)
  (define conf-list (filter (lambda (x) (equal? key (car x))) confs))
  (cond [(empty? conf-list)
         (raise
          (make-exn:fail
           (format "without ~v item in configuration file." key)
           (current-continuation-marks)))]
        [(not (empty? (cdr conf-list)))
         (raise
          (make-exn:fail
           (format "more than one ~v item in configuration file." key)
           (current-continuation-marks)))]
        [else
         (car conf-list)]))

(define (get-conf key confs)
  (car (get-conf* key confs)))

(define (get-conf* key confs)
  (cdr (fetch-config key confs)))


(module+ test
  (require rackunit)
  (test-case
      "Check to parse config file."
    (check-equal?
     (get-conf* 'db
                '((db
                   (server "localhost")
                   (port 3306)
                   (database "zhiling_test")
                   (user "zl_test")
                   (password "123456"))))
     '((server "localhost")
       (port 3306)
       (database "zhiling_test")
       (user "zl_test")
       (password  "123456")))
    (check-equal?
      (get-conf 'server
                (get-conf* 'db
                           '((db
                             (server "localhost")
                             (port 3306)
                             (database "zhiling_test")
                             (user "zl_test")
                             (password "123456")))))
      "localhost")))
