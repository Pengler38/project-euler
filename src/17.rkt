#lang racket

(require "utils.rkt")

; "and" is counted
; spaces and hyphens are not

; Note: Won't print zero
(define (print-1-digit n)
  (match n
         [0 ""]
         [1 "one"]
         [2 "two"]
         [3 "three"]
         [4 "four"]
         [5 "five"]
         [6 "six"]
         [7 "seven"]
         [8 "eight"]
         [9 "nine"]))

(define (print-teens n)
  (match n
         [10 "ten"]
         [11 "eleven"]
         [12 "twelve"]
         [13 "thirteen"]
         [14 "fourteen"]
         [15 "fifteen"]
         [16 "sixteen"]
         [17 "seventeen"]
         [18 "eighteen"]
         [19 "nineteen"]))

(define (print-2-digits n)
  (define-values (tens-digit single-digit) (quotient/remainder n 10))
  (cond [(< n 10) (print-1-digit single-digit)]
        [(< n 20) (print-teens n)]
        [else
          (define tens-string
            (match tens-digit
                   [2 "twenty"]
                   [3 "thirty"]
                   [4 "forty"]
                   [5 "fifty"]
                   [6 "sixty"]
                   [7 "seventy"]
                   [8 "eighty"]
                   [9 "ninety"]))
          (define dash (if (= 0 single-digit)
                           ""
                           "-"))
          (string-append tens-string dash (print-1-digit single-digit))]))

; Translates a 1 to 3 digit number to words
(define (print-3-digits n)
  (define-values (hundreds-digit rest-digits) (quotient/remainder n 100))
  (define hundreds-string
    (cond [(= 0 hundreds-digit) ""]
          [(= 0 rest-digits) (string-append (print-1-digit hundreds-digit) " hundred")]
          [else (string-append (print-1-digit hundreds-digit) " hundred and ")]))
  (string-append hundreds-string (print-2-digits rest-digits)))

; Prints up to 999,999
(define (print-num n)
  (if (= 0 n)
      "zero"
      (let*-values ([(thousands rest-digits) (quotient/remainder n 1000)]
                    [(thousands-string)
                     (cond [(= 0 thousands) ""]
                           [(= 0 rest-digits) (string-append (print-3-digits thousands) " thousand")]
                           [else (string-append (print-3-digits thousands) " thousand, ")])])
        (string-append thousands-string (print-3-digits rest-digits)))))

(let* ([nums (stream-map print-num (in-range 1 1001))]
       [count-letters (lambda
         (num)
         (length (remove* (list #\, #\  #\- ) (string->list num) )))]
       [answer (stream-fold (lambda (acc str) (+ acc (count-letters str)))
                            0
                            nums)])
  (test-on-eq (compose count-letters print-num) 342 23)
  (test-on-eq (compose count-letters print-num) 115 20)
  (display "Answer: ")
  (displayln answer))
