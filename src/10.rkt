#lang racket

(require "utils.rkt")

(define (solve n)
  (stream-fold + 0 (stream-takewhile (create-prime-stream) (lambda (e) (<= e n)))))

(displayln (solve 2000000))
