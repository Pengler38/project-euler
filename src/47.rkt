#lang racket

(require "utils.rkt")

(define num-distinct-factors
  (stream-map (lambda (n) (cons n
                                (length (remove-duplicates (prime-decomp n)))))
              (in-naturals 3)))

(define four-distinct-factors
  (stream-map car (stream-filter (lambda (p) (= (cdr p) 4))
                                 num-distinct-factors)))

(define (solve s a b c d)
  (if (= (+ a 3) (+ b 2) (+ c 1) d)
      a
      (solve (stream-rest s) b c d (stream-first s))))

(display "Answer: ")
(let ([s four-distinct-factors])
  (solve (stream-rest (stream-rest (stream-rest (stream-rest s))))
         (stream-first s)
         (stream-first (stream-rest s))
         (stream-first (stream-rest (stream-rest s)))
         (stream-first (stream-rest (stream-rest (stream-rest s))))))
