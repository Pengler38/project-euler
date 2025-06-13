#lang racket

(require "utils.rkt")

(define (solve n)
  (foldl + 0 (primes-sieve n)))

(displayln (solve 2000000))
