#lang racket

(require "utils.rkt")

(define memo-fac (memoize1 factorial))

; Turns out this really isn't necessary, normal k-combination using a non-memoized factorial is
; perfectly fast enough for this problem.
; Still, fast-k-combination massively cuts down the run time
(define (fast-k-combination n k)
  (/ (memo-fac n)
     (* (memo-fac k) (memo-fac (- n k)))))

(define (solve n r acc)
  (if (> n 100)
      acc
      (let-values ([(acc*) (if (> (fast-k-combination n r) 1000000)
                               (cons (cons n r) acc)
                               acc)]
                   [(n* r*) (if (= r n)
                                (values (+ n 1) 1)
                                (values n (+ r 1)))])
        (solve n* r* acc*))))

(define combos (solve 1 1 null))
(display "Answer: ")
(displayln (length combos))

