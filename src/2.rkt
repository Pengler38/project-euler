#lang racket

(define (sum-even-fib acc a b limit)
  (let* ([next (+ a b)]
         [acc2 (if (even? next) (+ acc next) acc)])
    (if (> next limit)
        acc
       (sum-even-fib acc2 b next limit))))

(define (solve)
  (sum-even-fib 0 0 1 4000000))

(println (solve))
