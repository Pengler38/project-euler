#lang racket

(require "utils.rkt")

; Returns the binary representation... Note that the output is technically a base 10 number so I can't use addition etc. on it lol
(define (binary n)
  (define-values (n-rest binary-digit) (quotient/remainder n 2))
  (if (= 0 n)
      0
      (+ binary-digit (* 10 (binary n-rest)))))

(define (palindromic? n)
  (define d (digits n))
  (equal? (reverse d) d))

(define (double-palindromic? n)
  (and (palindromic? n) (palindromic? (binary n))))

(test-on-eq double-palindromic? 585 #t)
(test-on-eq double-palindromic? 584 #f)

(define double-palindromics
  (stream->list (stream-filter double-palindromic? (in-range 1 1000000))))

(for-each display (list "Double palindromic numbers: " double-palindromics "\n"
                        "Answer: " (apply + double-palindromics) "\n"))
