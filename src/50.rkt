#lang racket

(require "utils.rkt")

(define maximum 1000000)

; Adds up to the maximum, then gradually backtracks until the running total is a prime
; This ensures it always gets the longest prime chain when starting from the nth-prime
(define (check nth-prime)

  (define (go nth-prime running-total primes)
    (define p (get-prime nth-prime))
    (if (>= (+ running-total p) maximum)
        (backtrack running-total primes)
        (go (+ nth-prime 1) (+ p running-total) (cons p primes))))

  (define (backtrack running-total primes)
    (if (is-prime? running-total)
        primes
        (backtrack (- running-total (first primes)) (rest primes))))

  (go nth-prime 0 null))

(define (solve)
  (define (go n max-chain)
    (if (>= (get-prime n) maximum)
        max-chain
        (let ([chain (check n)])
          (if (> (length chain) (length max-chain))
              (go (+ 1 n) chain)
              (go (+ 1 n) max-chain)))))
  (go 0 null))

(define longest (solve))
(display "Lenght of the longest prime-chain which adds to a prime below one million: ")
(displayln (length longest))
(display "Answer: ")
(display (apply + longest))

