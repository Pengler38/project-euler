#lang racket

(require "utils.rkt")

; Input the fractions as (numerator . denominator) pairs
(define (fraction-equal? a b)
  (define-values (num-a denom-a) (values (car a) (cdr a)))
  (define-values (num-b denom-b) (values (car b) (cdr b)))
  (and (not (= 0 denom-b))
       (not (= 0 denom-a))
       (or (and (= 0 num-a) (= 0 num-b))
           (if (= 0 num-b)
               (= (/ num-b num-a) (/ denom-b denom-a))
               (= (/ num-a num-b) (/ denom-a denom-b))))))

; PROBLEM: What if there's multiple of one digit?
; This could be a problem on something like 11/121, it reduces to both 1/12 and 1/21! My code would only check 1/12
; But: We are only using two digits in the numerator and denominator which makes it simple

; Returns a list of the digits in both a and b
(define (common-digits a b)
  (set-intersect (digits a) (digits b)))

(define (curious? numerator denominator)
  ; A list of the potential simplifications using the incorrect simplification method
  (define (get-potential-simplifications)
    (define num-dig (digits numerator))
    (define denom-dig (digits denominator))
    (map (lambda (d) (cons (undigits (remove d num-dig))
                           (undigits (remove d denom-dig))))
         (common-digits numerator denominator)))

  (not (null? (filter (lambda (s) (and (not (= numerator denominator))
                                       (fraction-equal? (cons numerator denominator) s)))
                      (get-potential-simplifications)))))

(define (trivial? numerator denominator)
  (and (= 0 (remainder numerator 10))
       (= 0 (remainder denominator 10))))

(define (get-curious)
  (define (go a b acc)
    (define acc* (if (and (curious? a b) (not (trivial? a b)))
                     (cons (cons a b) acc)
                     acc))
    (cond [(= a 100) acc]
          [(= b 100) (go (+ 1 a) a acc*)]
          [else (go a (+ 1 b) acc*)]))
  (go 10 10 null))

(test-on-eq (args->arglist curious?) `(49 98) #t)
(test-on-eq (args->arglist curious?) `(49 97) #f)

(define curious (get-curious))
(define answer
  (let* ([mult (foldl (lambda (acc n) (cons (* (car acc) (car n)) (* (cdr acc) (cdr n))))
                      `(1 . 1) curious)]
         [divisor (gcd (car mult) (cdr mult))])
    (/ (cdr mult) divisor)))

(for-each display
          (list "Curious fractions: " curious "\n"
                "Answer: " answer "\n"))
