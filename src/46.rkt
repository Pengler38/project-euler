#lang racket

(require "utils.rkt")

(define (conjecture-holds? n)
  (define (go sq)
    (define remainder (- n (* 2 (* sq sq))))
    (cond [(< remainder 2) #f]
          [(is-prime? remainder) #t]
          [else (go (+ sq 1))]))
  (go 0))

(define (solve n)
  (if (conjecture-holds? n)
      (solve (+ 2 n))
      n))

(display "Answer: ")
(solve 3)
