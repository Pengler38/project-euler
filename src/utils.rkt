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


(define (create-prime-stream)
  ; Returns a vector prime-sieve
  (define (prime-sieve length)
    (define vec (make-vector length #t))

    (define (remove-prime-multiples! vec prime)
      (define multiples (in-range (* 2 prime) length prime))
      (stream-for-each (lambda (x) (vector-set! vec x #f))
                       multiples))

    ; If n is prime, remove multiples. If not, do nothing.
    (define (sieve-n! n)
      (if (vector-ref vec n)
          (remove-prime-multiples! vec n)
          null))

    (stream-for-each sieve-n! (in-range 2 length))
    vec)

  (define (go i vec)
    (define (rest-of-stream) (go (+ 1 i) vec))

    (cond [(>= i (vector-length vec)) ; v is empty of primes. Re-eval with new primes.
           (go i (prime-sieve (* 2 (vector-length vec))))]

          [(vector-ref vec i) ; If prime, add to stream
           (stream-cons i (rest-of-stream))]

          [else ; v at i is not a prime, move to the next index
            (rest-of-stream)]))

  (go 2 (prime-sieve 1024)))

; Takes the first n values of s, returning it as a list and the remaining stream
; Returns (values list stream)
(define (stream-take->list s n)
  (define (go s n l)
    (if (<= n 0)
        (values (reverse l) s)
        (go (stream-rest s)
            (- n 1)
            (cons (stream-first s) l))))
  (go s n empty))


; Takes the first n values of s, returning it as a vector and the remaining stream
; Returns (values vector stream)
(define (stream-take->vector s n)
  (let ([initial-n n]
        [vec (make-vector n #f)])
    (define (go s n)
      (if (<= n 0)
          (values vec s)
          (let ()
            (vector-set! vec (- initial-n n) (stream-first s))
            (go (stream-rest s) (- n 1)))))
    (go s n)))

(struct p-cache (vec stream))
(define init-prime-cache
  (let ([s (create-prime-stream)])
    (p-cache (make-vector 1 (stream-first s)) (stream-rest s))))

; Gets the nth prime from the cache of prime
; If it's out of range, use prime-stream to add some more
(define (get-prime-cached prime-cache n)
  (match prime-cache
    [(p-cache vec s)
     (define vec-length (vector-length vec))
     (if (< n vec-length)
         ; n is in-range, dereference
         (values prime-cache (vector-ref vec n))
         ; n is out of range, make new cache with length doubled
         (let*-values ([(primes new-s) (stream-take->vector s vec-length)]
                       [(new-vec) (vector-append vec primes)]
                       [(new-prime-cache) (p-cache new-vec new-s)])
           (get-prime-cached new-prime-cache n)))]))

; Returns a procedure that gets a prime, caching the result
(define (create-get-prime)
  (let ([prime-cache init-prime-cache])
    (lambda (n)
            (let-values ([(new-prime-cache result) (get-prime-cached prime-cache n)])
              (set! prime-cache new-prime-cache)
              result))))

; Has interior mutation, caches results
(define get-prime (create-get-prime))

; Decomposes n to a list of the prime components
(define (prime-decomp n)
  (define (go n acc nth-prime)
    (define prime (get-prime nth-prime))
    (define-values (quot rem) (quotient/remainder n prime))
    (define-values (new-n new-acc new-nth-prime)
      (if (= 0 rem)
          (values quot (cons prime acc) nth-prime)
          (values n acc (+ 1 nth-prime))))
    (if (= 1 n)
        acc
        (go new-n new-acc new-nth-prime)))
  (go n empty 0))

(define (factorial n)
  (define (go acc n)
    (if (<= n 1)
        acc
        (go (* acc n) (- n 1))))
  (go 1 n))

; https://en.wikipedia.org/wiki/Divisor_function
; sigma0(n)
; For each prime that occurs n times in the decomposition, multiply by (n+1)
(define (num-divisors n)
  ; Counts the unique primes in the decomposition list
  ; e.g.
  ; `(3 3) -> `(2)
  ; `(3 2) -> `(1 1)
  (define (count-primes primes-list)
    (define prime-count 0)
    (define previous-prime -1)
    (define (go primes-list acc-list)
      (match primes-list
             [`() (cons prime-count acc-list)]
             [(list-rest first rest)
              (cond
                [(= previous-prime -1)
                 ; First iteration
                 (set! prime-count 1)
                 (set! previous-prime first)
                 (go rest acc-list)]
                [(= previous-prime first)
                 ; Prime is the same. Add to prime-count
                 (set! prime-count (+ 1 prime-count))
                 (go rest acc-list)]
                [else
                  ; Prime is different.
                  ; Add the current prime-count to acc-list, reset prime-count, new previous-prime
                  (define new-acc-list (cons prime-count acc-list))
                  (set! prime-count 1)
                  (set! previous-prime first)
                  (go rest new-acc-list)])]))
    (go primes-list empty))

  (foldl
    (lambda (n acc) (* acc (+ n 1)))
    1
    (count-primes (prime-decomp n))))

; The number of permutations that can be made by choosing k out of n items
; https://en.wikipedia.org/wiki/Permutation
(define (k-permutation n k)
  (/ (factorial n)
     (factorial (- n k))))

; The number of combinations that can be made by choosing k out of n items
; https://en.wikipedia.org/wiki/Combination
(define (k-combination n k)
  (/ (factorial n)
     (* (factorial k) (factorial (- n k)))))

 
; The number of combinations that can be made by choosing k out of n itemms,
; where the chosen item can be repeated up to k times
; https://en.wikipedia.org/wiki/Combination#Number_of_combinations_with_repetition
(define (k-multicombination n k)
  (k-combination (- (+ n k) 1)
                 k))
(define (test-on-eq f input correct-result)
  (display "Test f(")
  (display input)
  (display ") = ")
  (display correct-result)
  (displayln "?")
  (define result (f input))
  (if (equal? correct-result result)
      (display "  Passed, result: ")
      (display "  Failed, result: "))
  (displayln result))

(define (sum-1-to-n n)
  (/ (* n (+ n 1))
     2))
