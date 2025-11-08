#lang racket

(require "utils.rkt")

; Adds 2 to the pair fraction, then inverts is
(define (sqrt-iter p)
  (define a (car p))
  (define b (cdr p))
  (cons b (+ (* 2 b) a)))

; Adds 1 ot the pair fraction
(define (sqrt-finish p)
  (define a (car p))
  (define b (cdr p))
  (cons (+ b a) b))

(define expansion-stream
  (let ()
    (define (go p)
      (stream-cons (sqrt-finish p) (go (sqrt-iter p))))
    (go (cons 1 2))))

(display "Answer: ")
(stream-fold (lambda (acc p)
                     (if (> (length (digits (car p))) (length (digits (cdr p))))
                         (+ acc 1)
                         acc))
             0 (stream-take expansion-stream 1000))
