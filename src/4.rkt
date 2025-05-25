#lang racket

(define (is-palindrome? n)
  (let* ([str-n (number->string n)]
        [reverse-str-n (list->string(reverse (string->list str-n)))])
    (string=? str-n reverse-str-n)))

; Creates a list of product=n*n with a subtraction of diff split between the two numbers
(define (get-product-list diff n)
  (let* ([half-diff (/ diff 2)]
         [f-half-diff (floor half-diff)]
         [c-half-diff (ceiling half-diff)])
    (build-list
      (+ 1 f-half-diff)
      (lambda (x)
              (* (- (- n c-half-diff) x)
                 (+ (- n f-half-diff) x)))
    )
  ))

(define (largest-palindrome-product max)
  (define (go diff)
    (let* ([product-list (get-product-list diff max)]
           [first-product (findf is-palindrome? product-list)])
      (if (integer? first-product)
          first-product
          (go (+ diff 1)))))
  (go 0))

(display "Testing: ")
(println (largest-palindrome-product 99))

(display "Answer: ")
(println (largest-palindrome-product 999))
