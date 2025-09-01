#lang racket

(require "utils.rkt")

; For a 5x5, start at 1, +2 four times, + 4 four times, last is 5x5 = 25.
; On each wrap of the spiral, the side length increases by 2

(require "utils.rkt")

(define (spiral n)
  (define last(* n n))
  (define (go acc num square-length)
    (if (= num last)
        acc
        (let* ([corner-nums (map (lambda (k) (+ num (* k square-length)))
                                 `(1 2 3 4))]
               [sum-of-corners (apply + corner-nums)])
          (go (+ acc sum-of-corners)
              (+ (* 4 square-length) num)
              (+ 2 square-length)))))
  (go 1 1 2))

(test-on-eq spiral 5 101)
(display "Answer: ")
(displayln (spiral 1001))
