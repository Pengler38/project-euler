#lang racket

(require "utils.rkt")

; A cycle occurs if it's the same numerator divided by n
(define (unit-fraction-cycle-length denominator)
  ; Keep track of the numerators checked and the div-count that it was checked at
  (define map (make-hash))
  (define (go numerator div-count)
    (define next-numerator (remainder numerator denominator))
    (define map-ref-result (hash-ref map next-numerator `not-found))
    (cond
      ; If next-numerator is 0, return 0. Clean division with no cycles.
      [(= next-numerator 0) 0]
      ; No cycle yet, add the divcount to map with the numerator as key, and recurse
      [(equal? `not-found map-ref-result)
       (hash-set! map next-numerator div-count)
       (go (* 10 next-numerator) (+ 1 div-count))]
      ; Cycle found, return the current div count minus the div count which had the same numerator
      [else (- div-count map-ref-result)]))
  (go 1 0))

(define longest-cycle
  (let* ([cycles (stream-map unit-fraction-cycle-length (in-range 1 1000))]
         ; Add 1 because (in-range) starts at 1, not 0
         [cycles-max (+ 1 (stream-index-of-max cycles))])
    cycles-max))

(test-on-eq unit-fraction-cycle-length 1 0)
(test-on-eq unit-fraction-cycle-length 2 0)
(test-on-eq unit-fraction-cycle-length 6 1)
(test-on-eq unit-fraction-cycle-length 7 6)

(display "Answer: ")
(displayln longest-cycle)
