#lang racket

; Finds the largest factor
; All factors found are guaranteed to be prime since it checked all previous factors.
; The last one is the largest.
(define (largest-prime-factor initial-target)
  (define (go n target)
    (if (integer? (/ target n))
        (if (= 1 (/ target n))
            n
            (go (+ n 1) (/ target n))
            )
        (go (+ n 1) target)
        ))
  (go 2 initial-target))

(display "Testing: ")
(displayln (largest-prime-factor 13195))

(display "Answer: ")
(displayln (largest-prime-factor 600851475143))
