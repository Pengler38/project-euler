#lang racket

(require "utils.rkt")

(define (pentagonal n)
  (/ (* n (- (* 3 n) 1))
     2))

(define is-pentagonal? (create-is-f? pentagonal 1))

(define (solve n-max)
  (define (go a b ds)
    (letrec ([pentagonal-a (pentagonal a)]
             [pentagonal-b (pentagonal b)]
             [ds* (if (and (is-pentagonal? (- pentagonal-a pentagonal-b))
                           (is-pentagonal? (+ pentagonal-a pentagonal-b)))
                      (cons (cons a b) ds)
                      ds)])
      (cond [(>= a n-max) ds*]
            [(>= b (- a 1)) (go (+ 1 a) 1 ds*)]
            [else (go a (+ 1 b) ds*)])))
  (go 2 1 null))

(define answers
  (sort (solve 5000) (lambda (a b) (< (- (pentagonal (car a)) (pentagonal (cdr a)))
                                      (- (pentagonal (car b)) (pentagonal (cdr b)))))))

(define answer
  (- (pentagonal (car (first answers)))
     (pentagonal (cdr (first answers))) ))

(for-each display (list "n's where the pentagonals sum and differ to pentagonals: " answers "\n"
                        "Answer: " answer))


