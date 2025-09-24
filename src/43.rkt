#lang racket

(require "utils.rkt")

; Iterate through all the different permutations of 0-9 (3,628,800 numbers to check)

(define divisors `(17 13 11 7 5 3 2))

(define (funny-property? n*)
  (define (go n div-list)
    (cond [(null? div-list) #t]
          [(not (= 0 (remainder (remainder n 1000) (first div-list)))) #f]
          [else (go (quotient n 10) (rest div-list))]))
  (go n* divisors))

(test-on-eq funny-property? 1406357289 #t)

(define all-funny-numbers
  (filter funny-property?
          (map undigits (permutations (list 0 1 2 3 4 5 6 7 8 9)))))

(for-each display (list "All funny numbers: " all-funny-numbers "\n"
                        "Answer: " (apply + all-funny-numbers) "\n"))
