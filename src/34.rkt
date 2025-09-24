#lang racket

(require "utils.rkt")

(define (funny? n)
  (= n (foldl (lambda (n acc) (+ acc (factorial n)))
              0 (digits n))))

(test-on-eq funny? 145 #t)
(test-on-eq funny? 146 #f)

; Limiting this at 1,000,000. Seems like there are no funny numbers between 1,000,000 and 100,000,000
(define curious-nums
  (stream->list (stream-filter funny? (in-range 3 1000000))))

(for-each display (list "Curious nums: " curious-nums "\n"
                        "Answer: " (apply + curious-nums) "\n"))

