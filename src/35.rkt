#lang racket

(require "utils.rkt")

(define prime? (create-prime?))

(define (rotate n)
  (define d (digits n))
  (undigits (append (rest d) (list (first d)))))

(define (circular-prime? n)
  (define end (num-digits n))
  (define (go i n)
    (cond [(not (prime? n)) #f]
          [(>= i end) #t]
          [else (go (+ i 1) (rotate n))]))
  (go 1 n))

(define circular-primes (let ()
  (define (go s acc)
    (define acc* (if (circular-prime? (stream-first s))
                     (cons (stream-first s) acc)
                     acc))
    (if (< (stream-first s) 1000000)
        (go (stream-rest s) acc*)
        acc))
  (go (create-prime-stream) null)))

(test-on-eq circular-prime? 97 #t)
(test-on-eq circular-prime? 73 #t)
(test-on-eq circular-prime? 5 #t)
(test-on-eq circular-prime? 99 #f)

(for-each display (list "Circular primes: " circular-primes "\n"
                        "Answer: " (length circular-primes)))
