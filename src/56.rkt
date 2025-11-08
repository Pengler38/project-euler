#lang racket

(require "utils.rkt")

(define (max-digits)
  (define (go a b maximum)
    (define new-max (max (sum-of-digits (expt a b)) maximum))
    (cond [(< b 99) (go a (+ b 1) new-max)]
          [(< a 99) (go (+ a 1) 1 new-max)]
          [else new-max]))
  (go 1 1 0))

(display "Answer: ")
(max-digits)
