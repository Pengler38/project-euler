#lang racket

(require "utils.rkt")

(test-on-eq (compose sum-of-digits factorial) 10 27)

(display "Answer: ")
(displayln (sum-of-digits (factorial 100)))
