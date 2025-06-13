#lang racket

(define (pyth a b)
  (sqrt (+(* a a)
          (* b b))))

; If the adding the pythagorean triplet of a and b is less than 1000, add 1 to a and b
; If it is greater than 1000, subtract 1 from a.
; This will hopefully converge at the right answer :)
(define (solve a b)
  (let* ([c (pyth a b)]
         [sum (+ a b c)])
    (if (< sum 1000)
        (solve (add1 a) (add1 b))
        (if (> sum 1000)
            (solve (sub1 a) b)
            (list a b c)))))

(let ([answer (solve 0 0)])
  (displayln answer)
  (displayln (apply * answer)))
