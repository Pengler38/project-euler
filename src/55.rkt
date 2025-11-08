#lang racket

(require "utils.rkt")

(define (reverse-add n)
  (+ n (undigits (reverse (digits n)))))

(define (is-palindrome? n)
  (define (go a b)
    (cond [(and (null? a) (null? b)) #t]
          [(= (first a) (first b)) (go (rest a) (rest b))]
          [else #f]))
  (go (digits n) (reverse (digits n))))

(define (is-lychrel? n)
  (define (go n limit)
    (cond [(is-palindrome? n) #f]
          [(= 0 limit) #t]
          [else (go (reverse-add n) (- limit 1))]))
  (go (reverse-add n) 50))

(display "Answer: ")
(stream-length (stream-filter is-lychrel? (in-range 10000)))
