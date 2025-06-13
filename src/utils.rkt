#lang racket

(require racket/treelist)
(require racket/block)

(provide (all-defined-out))


; Checks if n is divisible by any num in ns
(define (divisible? n ns)
  (stream-ormap (lambda (x) (integer? (/ n x))) ns))

; Returns the first n primes in a treelist
; A treelist can be treated as a stream/sequence
(define (primes n)
  (define (primes_ length ns i)
    (if (= length n)
        ns
        (if (divisible? i ns)
            (primes_ length ns (+ i 1))
            (primes_ (+ length 1) (treelist-add ns i) (+ i 1))
            )))
  (primes_ 1 (treelist 2) 3))

; Finds the primes by using the Sieve of Eratosthenes
(define (primes-sieve n)
  (letrec ([sieve (make-vector n #t)]
           ; Finds the next #t in the sieve after i
           [next-prime (lambda (i)
                               (let ([i1 (add1 i)])
                                 (if (or (= i1 n)
                                         (vector-ref sieve i1))
                                     i1
                                     (next-prime i1))))]
           ; Sets all multiples of idx to #f in the sieve
           [set-mul-false! (lambda (idx)
                                   (define (go idx mul)
                                     (let ([target-idx (* idx mul)])
                                       (if (>= target-idx n)
                                           (void)
                                           (block
                                             (vector-set! sieve target-idx #f)
                                             (go idx (add1 mul))))))
                                   (go idx 2))]
           ; Removes all non-prime numbers from the sieve
           [do-primes-sieve! (lambda ()
                                     (define (go i)
                                       ; For every multiple of i, set that to #f in the sieve
                                       (set-mul-false! i)
                                       ; Recurse or return void
                                       (if (>= i n)
                                           (void)
                                           (go (next-prime i))))
                                     (go 2))]
           [list-from-sieve (lambda ()
                                    (define (go i ns)
                                      (if (>= i n)
                                          ns
                                          (if (vector-ref sieve i)
                                              (go (add1 i) (cons i ns))
                                              (go (add1 i) ns))))
                                    (go 2 empty))])
    (do-primes-sieve!)
    ; Make a list of primes from the vector
    (list-from-sieve)))
