#lang racket

; Simple, since racket uses big ints

(define (sum-of-digits n)
  (if (< n 10)
      n
      (let-values ([(rest-digits first-digit) (quotient/remainder n 10)])
        (+ first-digit (sum-of-digits rest-digits)))))

(display "Test: ")
(displayln (sum-of-digits (expt 2 15)))

(display "Answer: ")
(displayln (sum-of-digits (expt 2 1000)))
