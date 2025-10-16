#lang racket

(define (mult-same-digits? n max-mult)
  (define (count! arr x)
    (if (= x 0)
        null
        (let-values ([(rest digit) (quotient/remainder x 10)])
          (vector-set! arr digit (+ 1 (vector-ref arr digit)))
          (count! arr rest))))

  (define digit-arr (make-vector 10 0))
  (count! digit-arr n)

  (define (check-multiples i)
    (if (> i max-mult)
        #t
        (letrec ([mult-arr (make-vector 10 0)])
          (count! mult-arr (* i n))
          (if (equal? digit-arr mult-arr)
              (check-multiples (+ i 1))
              #f))))
  (check-multiples 2))

(define (solve n)
  (if (mult-same-digits? n 6)
      n
      (solve (+ 1 n))))

(display "Answer: ")
(displayln (solve 1))
