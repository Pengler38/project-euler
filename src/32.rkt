#lang racket

(require "utils.rkt")

(define (pandigital-product? a b)
  (define v (make-vector 9 #f))
  (define extra-digits? #f)
  (define prod (* a b))
  (define (check-digits n)
    (if (= 0 n)
        null
        (let-values ([(q r) (quotient/remainder n 10)])
          (if (or (= 0 r)
                  (vector-ref v (- r 1)))
              (set! extra-digits? #t)
              (vector-set! v (- r 1) #t))
          (check-digits q))))
  
  (for-each check-digits (list a b prod))
  ; It's pandigital if all digits 1-9 are used and there are no extra digits
  (and (= 0 (vector-length (vector-filter not v)))
       (not extra-digits?)))

(define (count-digits n)
  (if (= n 0)
      0
      (+ 1 (count-digits (quotient n 10)))))

; I want 9x9999 through 9999x9
; Only check digits which are total 5 digits

; Outputs all pandigital products, returns a list of pairs
(define (get-pandigital-products)
  (define pandig-products (list))
  (define (go a b)
    (define-values (next-a next-b)
      (if (and (< b 9999)
               (<= (+ (count-digits a) (count-digits b))
                   5))
          (values a (+ 1 b))
          (values (+ 1 a) 1)))
    (if (pandigital-product? a b)
        (set! pandig-products (cons (cons a b) pandig-products))
        null)
    (if (>= next-a 10000)
        null
        (go next-a next-b)))
  (go 1 1)
  pandig-products)

(test-on-eq (args->arglist pandigital-product?) `(39 186) #t)
(test-on-eq (args->arglist pandigital-product?) `(39 187) #f)

(displayln "Answer: ")
(let* ([pandigital-products (get-pandigital-products)]
       [products (map (lambda (p) (* (car p) (cdr p))) pandigital-products)]
       [answer (apply + (remove-duplicates products))])
  (displayln answer))
