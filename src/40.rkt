#lang racket

(require "utils.rkt")

(define irrational-decimal-stream (let ()
  (define (go ds n)
    (if (null? ds)
        (go (reverse (digits n)) (+ 1 n))
        (stream-cons (first ds) (go (rest ds) n))))
  (go (digits 1) 2)))

(define (nth-irrational-decimal n) (stream-ref irrational-decimal-stream (- n 1)))

(test-on-eq nth-irrational-decimal 12 1)
(test-on-eq nth-irrational-decimal 11 0)

(for-each display (list "Answer: "
                        (apply * (map nth-irrational-decimal `(1 10 100 1000 10000 100000 1000000)))
                        "\n"))
