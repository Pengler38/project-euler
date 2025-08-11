#lang racket

(require "utils.rkt")

(define test-input (list->vector (list
3
7 4
2 4 6
8 5 9 3
)))

(define real-input (list->vector (list
75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
)))

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
