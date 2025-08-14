#lang racket

; 7 Jan 1900 is a Sunday, start from there going week by week
; Ignore all first days in 1900, until we get to 1901

; Month 0 is treated as not existing
(define standard-days-in-month (list->vector (list #f 31 28 31 30 31 30 31 31 30 31 30 31)))

(define (days-in-month-given-leap leap? month)
  (if (and leap? (= month 2))
      29
      (vector-ref standard-days-in-month month)))

(define (days-in-month month year)
  (define leap? (if (and (= 0 (remainder year 4))
                         (or (not (= 0 (remainder year 100)))
                             (= 0 (remainder year 400))))
                    #t
                    #f))
  (days-in-month-given-leap leap? month))

(define (count-sundays)
  (define (go acc day month year)
    (define new-acc (if (and (= day 1) (>= year 1901)) ; Ignore 1st days until it's 1901!
                             (+ 1 acc)
                             acc))
      (define-values (new-day new-month new-year)
         (if (> day (days-in-month month year))
             ; Increment month, increment year if needed
             (values (- day (days-in-month month year))
                     (if (= month 12) 1 (+ 1 month))
                     (if (= month 12) (+ 1 year) year))
             ; Still the same month
             (values (+ day 7)
                     month
                     year)))
      (if (<= year 2000)
          (go new-acc new-day new-month new-year)
          acc))
    ; Start at 1/7/1900, the first sunday
    (go 0 7 1 1900))

  (display "Answer: ")
  (displayln (count-sundays))
