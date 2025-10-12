#lang racket

(require "utils.rkt")

(define ten-digits (expt 10 10))

(display "Answer: ")
(stream-fold (lambda (a b) (modulo (+ a b) ten-digits))
             0
             (stream-map (lambda (n) (power-mod n n ten-digits)) (in-range 1 1001)))
