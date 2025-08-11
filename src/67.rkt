; The same as problem 18, but with a different input

#lang racket

(require "utils.rkt")

(define test-input (list->vector (list
3
7 4
2 4 6
8 5 9 3
)))


(define real-input (list->vector (file->list "data/67.txt")))

(struct pos (y x))

(define (triangle-idx p)
  (match p
         [(pos y x) (+ x (sum-1-to-n y))]))

; Gets the correct value according to the x position and the y level on the triangle
; Warning: If the x position is out of range for the given y-level, it will still try to get a value
(define (triangle-get tri-vec p)
  (vector-ref tri-vec (triangle-idx p)))

(define (triangle-set! tri-vec p v)
  (vector-set! tri-vec (triangle-idx p) v))

(define (triangle-in-range tri-vec p)
  (if (< (triangle-idx p) (vector-length tri-vec))
      #t
      #f))

(define (child-right p)
  (match p
         [(pos y x) (pos (+ 1 y) (+ 1 x))]))

(define (child-left p)
  (match p
         [(pos y x) (pos (+ 1 y) x)]))

(define (max-path tri-vec)
  ; Use tri-cache to memoize calculated results
  (define tri-cache (make-vector (vector-length tri-vec) #f))
  (define (go p)
    (if (not (triangle-in-range tri-vec p))
        ; Not in range, return 0
        0
        ; In range, check cache
        (if (not (eq? #f (triangle-get tri-cache p)))
            ; In cache, use it
            (triangle-get tri-cache p)
            ; Not in cache, recurse on left and right. Add current value with the max between the left and right
            (let* ([left (go (child-left p))]
                   [right (go (child-right p))]
                   [answer (+ (triangle-get tri-vec p) (max left right))])
              (triangle-set! tri-cache p answer)
              answer))))
  (go (pos 0 0)))

(test-on-eq max-path test-input 23)
(max-path real-input)
