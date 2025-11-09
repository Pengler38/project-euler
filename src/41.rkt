#lang racket

(require "utils.rkt")

; 987654321, 87654321 and all combinations aren't prime, they're all divisible by 3
; This can be shown by using the trick where you add all the digits together and check if it's divisible by 3.
;
; Start at 7 digit pandigital-max
(define pandigital-max 7654321)

(define reversed-primes (let ()
  (define (go s l)
    (if (> (n-pandigital (stream-first s) 0))
        0
        null)
    (if (> (stream-first s) pandigital-max)
        l
        (go (stream-rest s) (cons (stream-first s) l))))
  (go (create-prime-stream (+ pandigital-max 10000)) null)))

(display "Answer: ")
(stream-first (stream-filter (lambda (n) (> (n-pandigital n) 0))
                             reversed-primes))
