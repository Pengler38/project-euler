#lang racket

(provide (all-defined-out))


; Adds to the prime-list by using a segmented sieve of Eratosthenes
(define (segmented-primes-sieve prime-list start [segment-size 1048576])
  (define sieve (make-vector segment-size #t))
  (define (set-mul-false! n)
    (stream-for-each (lambda (i) (vector-set! sieve (- i start) #f))
                     (in-range (* n (ceiling (/ start n)))
                               (+ start segment-size)
                               n)))
  (define (get-primes-from-sieve plist)
    (stream-fold (lambda (acc n)
                         (if (vector-ref sieve (- n start))
                             (let () ; Prime found, add to list and remove multiples from the sieve
                               (set-mul-false! n)
                               (cons n acc))
                             acc))
                 plist
                 (in-range start (+ start segment-size))))

  (for-each set-mul-false! prime-list)
  (cons (get-primes-from-sieve prime-list)
        (+ start segment-size)))

(define (create-prime-stream [segment-size 1048576])
  (define (get-new old-list list acc)
    (if (eq? list old-list)
        acc
        (get-new old-list (rest list) (cons (first list) acc))))
  (define (go primes new-primes i)
    (cond [(null? new-primes)
           (define p (segmented-primes-sieve primes i segment-size))
           (go (car p) (get-new primes (car p) null) (cdr p))]
          [else (stream-cons (first new-primes) (go primes (rest new-primes) i))]))
  (go null null 2))

(define (stream-takewhile s f)
  (if (f (stream-first s))
      (stream-cons (stream-first s) (stream-takewhile (stream-rest s) f))
      empty-stream))

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
; Output list starts with the largest number
; Slightly faster than prime-decomp
(define (reverse-prime-decomp n)
  (define (go n acc nth-prime)
    (define prime (get-prime nth-prime))
    (define-values (quot rem) (quotient/remainder n prime))
    (define-values (new-n new-acc new-nth-prime)
      (if (= 0 rem)
          (values quot (cons prime acc) nth-prime)
          (values n acc (+ 1 nth-prime))))
    (if (<= n 1)
        acc
        (go new-n new-acc new-nth-prime)))
  (go n empty 0))

; Decomposes n to a list of the prime components
; Output list starts with the smallest number
(define (prime-decomp n)
  (reverse (reverse-prime-decomp n)))

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

(define (sum-of-digits n)
  (if (< n 10)
      n
      (let-values ([(rest-digits first-digit) (quotient/remainder n 10)])
        (+ first-digit (sum-of-digits rest-digits)))))

; All numbers that divide n without remainder
; WARNING: divisors are unordered due to (combinations), EXCEPT the first will always be n and the last will always be 1
; WARNING: Perhaps not as efficient as it could be, (combinations) creates duplicates which must be removed due to duplicate numbers in the prime decomposition
(define (divisors n)
  (remove-duplicates (map (lambda (l) (foldl * 1 l)) (combinations (prime-decomp n)))))

; All numbers less than n that divide n without remainder
(define (proper-divisors n)
  (define d (divisors n))
  (take d (- (length d) 1)))

; Takes O(n) time, n being the length of the input list
(define (list-pop list idx)
  (define (go list n acc-list value)
    (if (null? list)
        (values value (reverse acc-list))
        (if (= 0 n)
            ; Remove the current value, and save it in value to be returned later
            ; Don't add the current value to the acc-list
            (go (cdr list) (- n 1) acc-list (car list))
            ; Iterate as normal. Add first to acc-list, keep value the same
            (go (cdr list) (- n 1) (cons (car list) acc-list) value))))
  (go list idx empty `error-idx-is-past-list-length))

; Creates a stream of the fibonacci numbers
(define (fib-stream)
  (define a 0)
  (define b 1)
  (define (fib)
    (define next (+ a b))
    (set! a b)
    (set! b next)
    (stream-cons next (fib)))
  (stream-cons 1 (fib)))

; Returns the index of the maximum number
(define (stream-index-of-max stream)
  (define result-list
    (stream-fold
      (lambda (acc n)
              (match acc [(list current-idx maximum maximum-idx)
                          (if (> n maximum)
                              (list (+ 1 current-idx) n current-idx)
                              (list (+ 1 current-idx) maximum maximum-idx))]))
      (list 0 0 0)
      stream))
  (third result-list))

; Creates a function, prime?, which has an internal cache of primes that gets updated when needed
(define (create-prime?)
  (define prime-stream (create-prime-stream))
  (define max-prime 0)
  (define map (make-hash))
  (define (prime? n)
    (if (< n max-prime)
        ; Prime would be in the map if it's prime
        (hash-ref map n #f)
        ; Prime not in map, add more primes to map
        (let ()
          (set! max-prime (stream-first prime-stream))
          (set! prime-stream (stream-rest prime-stream))
          (hash-set! map max-prime #t)
          ; Recurse / try again
          (prime? n))))
  prime?)

; Turns f from a function that takes multiple args to a function that takes a list of args
(define (args->arglist f)
  (lambda (l) (apply f l)))

; Finds the greatest common divisor of a and b
(define (gcd a b)
  (define (go as bs)
    (cond
      [(or (null? as) (null? bs)) 1]
      [(= (first as) (first bs)) (first as)]
      ; Recurse and iterate through the left list or the right list
      [(> (first as) (first bs)) (go (rest as) bs)]
      [else (go as (rest bs))]))
  (go (sort (divisors a) >) (sort (divisors b) >)))

; Finds the least common multiple of a and b
(define (lcm a b)
  (/ (* a b)
     (gcd a b)))

; Outputs a list of the digits, starting with the least significant digit
(define (digits n)
  (define-values (q r) (quotient/remainder n 10))
  (if (= 0 q)
      (list r)
      (cons r (digits q))))

; Outputs a number from a list of the digits, which starts with the least significant digit
(define (undigits ds)
  (if (null? ds)
      0
      (+ (first ds) (* 10 (undigits (rest ds))))))

(define (num-digits n)
  (if (= 0 n)
      0
      (+ 1 (num-digits (quotient n 10)))))

; Returns which n-pandigital the nums are (contains all digits 1-n with no extra digits or zeroes)
(define (n-pandigital . xs)
  (define vec (make-vector 9 #f))
  (define extra? #f)
  (define zero? #f)
  (define (check-digit n)
    (cond [(= 0 n) (set! zero? #t)]
          [(vector-ref vec (- n 1)) (set! extra? #t)]
          [else (vector-set! vec (- n 1) #t)]))

  (for-each (lambda (n) (for-each check-digit (digits n)))
            xs)
  (define vec-list (vector->list vec))
  (define first-missing-digit
    (let ([i (index-where vec-list not)])
      ; If all the digits are covered, index-where returns #f and we replace #f with 9
      (if (number? i) i 9)))
  (define rest-missing? (= 0 (length (filter identity (drop vec-list first-missing-digit)))))

  (if (or extra? zero? (not rest-missing?))
      0
      first-missing-digit))

; Checks if the args combined together are pandigital (contain all digits 1-9 with no extras and no zeroes)
(define (pandigital? . ns)
  (= 9 (apply n-pandigital ns)))

; A higher order function which takes f, and returns an is-f? function which checks if a number is an output of f
; input to f starts with start, and each output of f must be increasing
; This can be used for prime numbers, triangle numbers, etc.
(define (create-is-f? f start)
  (define hmap (mutable-set))
  (define i start)
  (define max-value 0)
  (define (is-f n)
    (if (<= n max-value)
        (set-member? hmap n)
        (let ([value (f i)])
          (set-add! hmap value)
          (set! max-value value)
          (set! i (+ i 1))
          (is-f n))))
  is-f)

(define is-prime?
  (let ([pstream (create-prime-stream)])
    (create-is-f? (lambda (_) ; This lambda is a hacky way to bypass the caching on get-prime so only create-is-f? caches results
                          (define r (stream-first pstream))
                          (set! pstream (stream-rest pstream))
                          r)
                  0)))

; Modular exponentiation
; Returns n^p mod m
(define (power-mod n p m)
  (define (go res base p)
    (cond [(= p 1) (modulo (* res base) m)]
          ; p is even, can optimize by squaring the base and halving p
          [(= 0 (modulo p 2))
           (go res
               (modulo (* base base) m)
               (quotient p 2))]
          ; p is odd, only do one power
          [else
            (go (modulo (* res base) m)
                base
                (- p 1))]))
  (go 1 n p))

; Memoizes a function f which takes exactly one argument (that argument must be hashable)
(define (memoize1 f)
  (define hmap (make-hash))
  (define (memoized-f x)
    (define hash-result (hash-ref hmap x #f))
    (if (eq? #f hash-result)
        (let ([result (f x)])
          (hash-set! hmap x result)
          result)
        hash-result))
  memoized-f)
