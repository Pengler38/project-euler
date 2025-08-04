#lang racket

(define collatz-cache (make-hash))

; Calculates the length of a collatz sequence and caches the results
(define (collatz n)
  (define (go counter n)
    (define cache-result (hash-ref collatz-cache n -1))
    (if (not (= -1 cache-result))
        ; Exists in the cache
        cache-result
        ; Calculate and add to cache
        (let ()
          (define answer
            (cond [(= 1 n) 1]
                  [(even? n) (+ 1 (go (+ 1 counter) (/ n 2)))]
                  [else (+ 1 (go (+ 1 counter) (+ 1 (* 3 n))))]))
          (hash-set! collatz-cache n answer)
          answer)))
  (go 1 n))

(define (find-idx-of-max list)
  (define (go list max max-idx idx)
    (cond
      [(empty? list) max-idx]
      [(> (first list) max) (go (rest list) (first list) idx (+ 1 idx))]
      [else (go (rest list) max max-idx (+ 1 idx))]))
  (go list (first list) 0 0))

(let* ([chain-lengths (map collatz (range 1 1000000))])
  (displayln (+ 1 (find-idx-of-max chain-lengths))))
