#lang racket

(require "utils.rkt")

(define (triangle n)
  (/ (* n (+ n 1))
     2))

(define (pentagonal n)
  (/ (* n (- (* 3 n) 1))
     2))

(define (hexagonal n)
  (* n (- (* 2 n) 1)))

(define is-triangle? (create-is-f? triangle 1))
(define is-pentagonal? (create-is-f? pentagonal 1))
(define is-hexagonal? (create-is-f? hexagonal 1))

(define (solve n)
  (let ([t (triangle n)])
    (if (and (is-triangle? t)
             (is-pentagonal? t)
             (is-hexagonal? t))
        n
        (solve (+ n 1)))))

; Start looking at 286 to skip 285, the first triangle number that's pentagonal and hexagonal
(let ([n (solve 286)])
  (for-each display (list "n: " n "\n"
                          "Triangle number: " (triangle n))))
