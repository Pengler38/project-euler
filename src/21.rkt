#lang racket

(require "utils.rkt")

(define (d n)
  (apply + (proper-divisors n)))

(define (amicable? n)
  (define dn (d n))
  (and (= n (d dn))
       (not (= n dn))))

(test-on-eq amicable? 220 #t)
(test-on-eq amicable? 284 #t)

(display "Answer: ")
(stream-fold (lambda (acc n) (if (amicable? n)
                                 (+ n acc)
                                 acc))
             0
             (in-range 1 10000))
