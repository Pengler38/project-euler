#lang racket

(require "utils.rkt")

(define prime? (create-prime?))

(define (prime-right? n)
  (cond [(= n 0) #t]
        [(not (prime? n)) #f]
        [else (prime-right? (quotient n 10))]))

(define (prime-left? n)
  (define (go exponent)
    (cond [(= exponent 0) #t]
          [(not (prime? (remainder n (expt 10 exponent)))) #f]
          [else (go (- exponent 1))]))
  (go (num-digits n)))

(define (truncatable-prime? n)
  (and (prime-right? n) (prime-left? n)))

(test-on-eq truncatable-prime? 3797 #t)
(test-on-eq truncatable-prime? 7927 #f)

(define truncatable-primes
  (stream->list (stream-filter truncatable-prime? (in-range 10 1000000))))

(for-each display (list "Truncatable primes: " truncatable-primes "\n"
                        "Answer: " (apply + truncatable-primes) "\n"))
