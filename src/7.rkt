#lang racket

(define (divisible? n ns)
  (ormap (lambda (x) (integer? (/ n x))) ns))

; Creates the first n primes
(define (primes n)
  (define (primes_ length ns i)
    (if (= length n)
        ns
        (if (divisible? i ns)
            (primes_ length ns (+ i 1))
            (primes_ (+ length 1) (append ns (list i)) (+ i 1))
            )))
  (primes_ 1 (list 2) 3))

(define (solve n)
  (last (primes n)))

(display "Testing: ")
(println (solve 6))

(display "Answer: ")
(println (solve 10001))
