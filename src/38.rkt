#lang racket

(require "utils.rkt")

(define max-pandigital 987654321)
; The maximum number to test is 10000 because 5 digits concatenated
; with 5 digits makes 10, which is too many digits to be pandigital
(define max-to-test 10000)

(define (concat-prod number n)
  (string->number (foldl (lambda (x acc)
                                 (string-append acc (number->string (* number x))))
                         "" (range 1 (+ n 1)))))

(define (get-concat-prod-pandigitals)
  (define (go acc number n)
    (define concat-prod-num (concat-prod number n))
    (define num-pandigital? (and (<= concat-prod-num max-pandigital)
                                 (pandigital? concat-prod-num)))

    (cond [(> number max-to-test) acc]
          [num-pandigital?
            (go (cons concat-prod-num acc) (+ 1 number) 2)]
          [(or (> concat-prod-num max-pandigital)
               (= n 9))
           (go acc (+ 1 number) 2)]
          [else (go acc number (+ n 1))]))
  (go null 1 2))

(define f (args->arglist (compose1 pandigital? concat-prod)))
(test-on-eq f (list 192 3) #t)
(test-on-eq f (list 1 9) #t)

(define concat-prod-pandigitals (get-concat-prod-pandigitals))
(for-each display (list "Concatenated product pandigitals: " concat-prod-pandigitals "\n"
                        "Answer: " (foldl max 0 concat-prod-pandigitals) "\n"))

