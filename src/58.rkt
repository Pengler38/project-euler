#lang racket

(require "utils.rkt")

(define diag-stream
  (let ()
    (define (go n side-length)
      (define corner-to-corner (- side-length 1))
      (define (p a) (cons a side-length))
      (stream-cons (p n)
        (stream-cons (p (+ corner-to-corner n))
          (stream-cons (p (+ (* 2 corner-to-corner) n))
            (stream-cons (p (+ (* 3 corner-to-corner) n))
              (go (+ (* 4 corner-to-corner) 2 n) (+ side-length 2)))))))
    (stream-cons (cons 1 1) (go 3 3))))

(define (boring-is-prime? n)
  (call/cc (lambda (return)
                   (stream-for-each (lambda (i) (if (= 0 (remainder n i))
                                                    (return #f)
                                                    (void)))
                                    (in-range 2 (ceiling (+ 0.5 (sqrt n)))))
                   #t)))

(define (solve)
  (define (go s primes total-numbers)
    (define new-primes (if (boring-is-prime? (car (stream-first s)))
                           (+ primes 1)
                           primes))
    (define new-total-numbers (+ total-numbers 1))
    (if (<= (/ new-primes new-total-numbers) 0.10)
        (cdr (stream-first s))
        (go (stream-rest s) new-primes new-total-numbers)))
  (go (stream-rest diag-stream) 0 1))

(display "Answer: ")
(solve)
