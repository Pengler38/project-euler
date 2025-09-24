#lang racket

(require "utils.rkt")

; Expects c > a, c > b
(define (right-triangle? a b c)
  (= (+ (* a a) (* b b))
     (* c c)))

(define (triangle-length-solutions p)
  (define (go acc a b c)
    (define acc* (if (right-triangle? a b c)
                     (cons (list a b c) acc)
                     acc))
    (cond [(<= c (/ p 3)) acc]
          [(< b a) (go acc 1 (- p c ) (- c 1))]
          [else (go acc* (+ a 1) (- b 1) c)]))
  (go null 1 1 (- p 2)))

(test-on-eq triangle-length-solutions 120 `((30 40 50) (24 45 51) (20 48 52)))

(define solution-stream (stream-map triangle-length-solutions (in-range 0 1000)))
(define p (stream-index-of-max (stream-map length solution-stream)))
(define solutions (stream-ref solution-stream p))

(for-each display (list "Highest Number Solutions: " (length solutions) "\n"
                        "Solutions: " solutions "\n"
                        "Perimeter: " p "\n"))
