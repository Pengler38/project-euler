#lang racket

(require "utils.rkt")

(define idx-first-1000-digit-fib
  (let ()
    (define target (expt 10 999))
    (define (go idx str)
      (if (>= (stream-first str) target)
          idx
          (go (+ 1 idx) (stream-rest str))))
    (go 1 (fib-stream))))

(display "Answer: ")
(displayln idx-first-1000-digit-fib)
