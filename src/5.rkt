#lang racket

(define (list-1-to-n n)
  (build-list n (lambda (x) (+ x 1))))

; Returns a list of prime numbers n decomposes into
(define (prime-decomposition n)
  (define (go n div ns)
    (if (= n 1)
        ns
        (if (integer? (/ n div))
            (go (/ n div) div (cons div ns))
            (go n (+ div 1) ns)
            )))
  (go n 2 empty))

; Combines the lists so that duplicates between the lists are ignored
; e.g. (list (list 5 2 2) (list 5 3 2)) -> (list 5 3 2 2)
; ns in nss must be sorted largest first
; ... There's probably a better way to do this...
(define (join-decomp nss)
  ; as and bs must be sorted largest first
  ; What a total mess of a function. Not sure how to make it work better.
  (define (combine as bs)
    (define (go as bs ns)
      (match (cons as bs)
             [(cons aa bb) #:when (and (empty? aa) (empty? bb))
              ns]
             [(cons aa bb) #:when (empty? aa)
              (append (reverse bb) ns)]
             [(cons aa bb) #:when (empty? bb)
              (append (reverse aa) ns)]
             [(cons aa bb)
              (match (cons (car aa) (car bb))
                     [(cons a b) #:when (= a b)
                      (go (cdr as) (cdr bs) (cons a ns))]
                     [(cons a b) #:when (> b a)
                      (go as (cdr bs) (cons b ns))]
                     [(cons a b) #:when (> a b)
                      (go (cdr as) bs (cons a ns))]
                     )]
             ))
    (reverse (go as bs empty)))
  (foldl combine empty nss))

(define (solve n)
  (let* ([decomp-lists (map prime-decomposition (list-1-to-n n))]
        [decomp-list (join-decomp decomp-lists)]
        [result (foldl * 1 decomp-list)])
    result))

(display "Testing: ")
(println (solve 10))

(display "Answer: ")
(println (solve 20))
