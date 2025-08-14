#lang racket

; Basic idea: Find all abundant numbers under 28123
; Do n^2 loop and add all combinations up, putting the results in a set
; Take all the numbers from 0 to 28123 that are not in the set

(require "utils.rkt")

(define (abundant? n)
  (if (> (apply + (proper-divisors n))
         n)
      #t
      #f))

(define (all-abundant-under-n n)
  (stream->list (stream-filter abundant? (in-range 1 n))))

(define (set-of-two-sums list)
  (define s (mutable-set))
  (define (go list)
    (if (null? list)
        s
        (let* ([first (car list)]
               [rest (cdr list)])
          (for-each (lambda (n) (set-add! s (+ first n)))
                    list)
          (go rest))))
  (go list))

; Sums of all two abundant numbers under 28124
(define abundant-sums
  (set-of-two-sums (all-abundant-under-n 28124)))

(define not-abundant-sums
  (let ([nums (list->set (range 0 28124))])
    (set-subtract nums abundant-sums)))

(display "Answer: ")
(displayln (apply + (set->list not-abundant-sums)))

