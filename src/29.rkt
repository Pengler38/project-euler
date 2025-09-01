#lang racket

(require "utils.rkt")

(define (solve last-n)
  ; map to hold the distinct terms in a^b
  (define map (make-hash))
  ; iterate through all numbers, adding them to the map
  (define (go a b)
    (hash-set! map (expt a b) #t)
    (if (and (= a last-n) (= b last-n))
        (hash-count map)
        (if (= b last-n)
            (go (+ 1 a) 2)
            (go a (+ 1 b)))))
  (go 2 2))

(test-on-eq solve 5 15)

(display "Answer: ")
(displayln (solve 100))
