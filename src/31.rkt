#lang racket

(require "utils.rkt")

(define coin-values (list 200 100 50 20 10 5 2 1))

; Returns the number of ways n can be made from the list coins
; Recurse in a tree-like fashion, try different numbers of coins and checking the remainders
(define (ways coins n)
  (if (null? coins)
      0
      (letrec
        (; Returns a list of the remainders after removing coin from n a variable number of times
         [coin-remainder
         (lambda (coin n)
                 (cond [(= n 0) null]
                       [(< n coin) (list n)]
                       [else (cons n (coin-remainder coin (- n coin)))]))]
         [recurse (lambda (new-n) (ways (rest coins) new-n))]
         [rest-ways (map recurse (coin-remainder (first coins) n))]
         [this-way (if (= 0 (remainder n (first coins)))
                       1
                       0)])
        (+ this-way (apply + rest-ways)))))

(test-on-eq (args->arglist ways) `((100) 200) 1)
(test-on-eq (args->arglist ways) `((100 10) 100) 2)
(test-on-eq (args->arglist ways) `((10 2) 20) 3)

(display "Answer: ")
(displayln (ways coin-values 200))
