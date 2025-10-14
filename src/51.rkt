#lang racket

(require "utils.rkt")

; Returns a list of the primes in the family
(define (primes-in-family f)
  (define (go i acc)
    (if (= i 10)
        acc
        (let ([p (string->number (string-replace (string-copy f)
                                                 "*"
                                                 (number->string i)))])
          (if (is-prime? p)
              (go (+ i 1) (cons p acc))
              (go (+ i 1) acc)))))
  ; If the family has a leading zero, skip the zero part of the family.
  ; 03 is not the same as 3!
  (if (equal? (string-ref f 0) #\*)
      (go 1 null)
      (go 0 null)))

; Returns a list of the families of n to search
(define (get-families n)
  ; Essentially make all combinations of the digits with wildcards for the digits left out of the combination
  ; Count i from 1 to 2^(num-digits n)
  (define n-string (number->string n))
  (define stop (expt 2 (num-digits n)))
  (define (go i acc)
    (if (= i stop)
        acc
        (let ([family (string-copy n-string)])

          ; Put the wildcards in family according to where the 1's would be in i's binary form
          (define (put-wilds-in-family! j pos)
            (if (= j 0)
                null
                (let ()
                  (cond [(= (remainder j 2) 1)
                         (string-set! family pos #\*)])
                  (put-wilds-in-family! (quotient j 2) (+ pos 1)))))

          (put-wilds-in-family! i 0)
          (go (+ i 1) (cons family acc)))))
  (go 1 null))

; Finds the first prime family of size s
(define (prime-family s)
  ; Tracks the prime families that don't need to be checked again
  (define family-checked (mutable-set))
  (define (go n max-family)
    (define prime-families
      (map (lambda (f)
                   (set-add! family-checked f)
                   (primes-in-family f))
           (filter (lambda (f) (not (set-member? family-checked f)))
                   (get-families (get-prime n)))))

    ; Find the maximum length family
    (define p (foldl (lambda (f acc)
                             (let ([f-len (length f)]
                                   [max-len (car acc)])
                               (if (> f-len max-len)
                                   (cons f-len f)
                                   acc)))
                     (cons 0 null)
                     (cons max-family prime-families)))
    (define max-family*-len (car p))
    (define max-family* (cdr p))

    (if (>= max-family*-len s)
        max-family*
        (go (+ n 1) max-family*)))
  (go 0 null))

(define family-8 (reverse (prime-family 8)))
(for-each display (list "The first prime family of size 8: " family-8 "\n"
                        "Answer: " (first family-8) "\n"))
