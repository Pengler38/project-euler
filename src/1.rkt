#lang racket

(define (is-mul-3-or-5? n)
  (or (= 0 (modulo n 5))
      (= 0 (modulo n 3)) ) )

(define (sum ns)
  (foldl + 0 ns))

(define (solve)
  (sum (filter is-mul-3-or-5? (build-list 1000 values))))

(println (solve))
