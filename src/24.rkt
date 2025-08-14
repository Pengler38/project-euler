#lang racket

#|
I'm not sure if this is the proper way to generate permutations, I just found a procedural way to do it
after staring at the lexicographic permutation of lists `(0 1 2) and `(0 1 2 3) long enough

       012   021   102   120   201   210
n:     1     2     3     4     5     6
index: 0     1     2     3     4     5

Example how to get a lexicographic permutation:
permutation 22 of `(0 1 2 3):
first take idx: (n-1) / 6 = 21/6 = 3
(The divisor, 6, is the factorial of the current list's length minus 1)
3, remaining list: `(0 1 2)

second take idx: ((n-1) % 6) / 2 = 21%6 / 2 = 3 / 2 = 1
31, remaining list: `(0 2)

third take idx: ((n-1) % 6) % 2) / 1 = 1
312, remaining list: `(0)

fourth take idx: 0
3120
|#

(require "utils.rkt")

; list must already be sorted to provide the proper lexicographic permutation
(define (nth-lexicographic-permutation n list)
  (define (go n list acc-list)
    (if (null? (cdr list))
        ; If there's only one element left, return that element cons acc-list
        (cons (car list) acc-list)
        ; Otherwise, recurse
        (let*-values
          ([(n-quot n-rem) (quotient/remainder n (factorial (- (length list) 1)))]
           [(value new-list) (list-pop list n-quot)])
          (go n-rem new-list (cons value acc-list)))))

  (reverse (go (- n 1) list empty)))

(test-on-eq (lambda (l) (apply nth-lexicographic-permutation l))
            `(5 (0 1 2))
            `(2 0 1))

; The last nth-nth-lexicographic-permutation should be exactly reversed!
(test-on-eq (lambda (l) (apply nth-lexicographic-permutation l))
            (list (k-permutation 10 10) `(0 1 2 3 4 5 6 7 8 9))
            `(9 8 7 6 5 4 3 2 1 0))

(display "Answer: ")
(displayln (nth-lexicographic-permutation 1000000 `(0 1 2 3 4 5 6 7 8 9)))

