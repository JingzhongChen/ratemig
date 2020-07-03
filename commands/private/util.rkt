#lang racket/base

(require racket/contract)

(provide (all-defined-out))

;; (provide/contract
;;  [inc (integer? . -> . integer?)])

(define (inc int)
  (+ int 1))

(define (not-null? v)
  (if (null? v)
      #f
      #t))

(define (not-empty? v)
  (not-null? v))

(define (not-equal? v v2)
  (if (equal? v v2)
      #f
      #t))

(module+ test
  (require rackunit)
  (test-case
      "test not-equal?"
    (check-true (not-equal? "a" "b"))
    (check-false (not-equal? "a" "a"))))

(define (chinese-date-format-str date)
  (string-append
   (number->string (date-year date))
   "-"
   (two-digit-str (date-month date))
   "-"
   (two-digit-str (date-day date))
   " "
   (two-digit-str (date-hour date))
   ":"
   (two-digit-str (date-minute date))
   ":"
   (two-digit-str (date-second date))))

(define (current-chinese-date-format-str)
  (chinese-date-format-str
   (seconds->date (current-seconds))))

(define (from-year-to-second-number date)
  (string->number
   (string-append
    (number->string (date-year date))
    (two-digit-str (date-month date))
    (two-digit-str (date-day date))
    (two-digit-str (date-hour date))
    (two-digit-str (date-minute date))
    (two-digit-str (date-second date)))))

(define (current-from-year-to-second-number)
  (from-year-to-second-number
   (seconds->date (current-seconds))))

(define (two-digit-str num)
  (cond [(< num 0)
         (raise (make-exn:fail:unsupported
                 "Unsupport navigative number."
                 (current-continuation-marks)))]
        [(< num 10)
         (string-append "0" (number->string num))]
        [(< num 100 )
         (number->string num)]
        [else
         (raise (make-exn:fail:unsupported
                 "Unsuppor number which larger than 100."
                 (current-continuation-marks)))]))


(module+ test
  (require rackunit)

  (test-case
      "test two-digit-str"
    (check-equal? (two-digit-str 9) "09")
    (check-equal? (two-digit-str 19) "19")
    (with-handlers ([exn:fail:unsupported?
                     (lambda (e)
                       (check-true #t))])
      (two-digit-str 109)
      (check-true #f))
    (with-handlers ([exn:fail:unsupported?
                     (lambda (e)
                       (check-true #t))])
      (two-digit-str -100)
      (check-true #f)))

  (test-case
      "test from-year-to-second-number"
    (define a-date
      (make-date 2 8 9 28 8 2018 2 239 #f 0))
    (check-equal? (from-year-to-second-number a-date) 20180828090802)))
