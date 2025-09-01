#lang racket

(require "utils.rkt")

(define (sum-of-power-digits n power)
  (if (= n 0)
      0
      (+ (expt (remainder n 10) power)
         (sum-of-power-digits (quotient n 10) power))))

(define (is-sum-of-digits? n power)
  (= n (sum-of-power-digits n power)))

; I only have to test up to where the digits at 9^5 added are always less than the digit itself
; 9^5 > 9
; 2 * 9^5 > 99
; 6 * 9^5 < 999,999

(define fifth-power-digit-sums
  (stream->list (stream-filter (lambda (n) (is-sum-of-digits? n 5))
                               (in-range 2 1000000))))

(test-on-eq (args->arglist sum-of-power-digits) `(1634 4) 1634)
(test-on-eq (args->arglist sum-of-power-digits) `(8208 4) 8208)
(test-on-eq (args->arglist sum-of-power-digits) `(9474 4) 9474)

(for-each display
          (list "Fifth digit sums: " fifth-power-digit-sums "\n"
                "Answer: " (apply + fifth-power-digit-sums) "\n"))
