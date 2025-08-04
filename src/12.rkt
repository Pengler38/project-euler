#lang racket

(require "utils.rkt")

(define (create-tri-stream)
  (define (go i acc)
    (stream-cons acc (go (+ 1 i)
                         (+ acc i))))
  (go 2 1))

(define (solve target-divisors)
  (define answer-stream
    (stream-filter (lambda (num) (>= (num-divisors num) target-divisors))
                   (create-tri-stream)))

  (stream-first answer-stream))

(display "Test: ")
(displayln (solve 5))
(display "Answer: ")
(displayln (solve 500))
