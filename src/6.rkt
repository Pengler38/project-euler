#lang racket

(define (square x) (* x x))

(define (solve n)
  (let* ([1-to-n (build-list n (lambda (x) (+ x 1)))]
         [sum-square (foldl (lambda (x acc) (+ (square x) acc)) 0 1-to-n)]
         [square-sum (square (foldl + 0 1-to-n))])
         (- square-sum sum-square)))

(display "Testing: ")
(println (solve 10))

(display "Answer: ")
(println (solve 100))
