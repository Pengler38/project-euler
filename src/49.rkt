#lang racket

(require "utils.rkt")

(define (prime-permutations n)
  (filter is-prime? (map undigits (permutations (digits n)))))

; Finds the first arithmetic sequence of length 3 in the list ns
; If no sequence is found, returns null
(define (find-3-arithmetic-sequence ns)
  (define n-set (list->set ns))
  (define (go ns ms)
    (cond [(null? ns) null]
          [(null? ms) (go (rest ns) (rest ns))]
          [(= (first ns) (first ms)) (go ns (rest ms))]
          [else
            (define third-num (+ (first ms)
                                 (- (first ms) (first ns))))
            (if (set-member? n-set third-num)
                (list (first ns) (first ms) third-num)
                (go ns (rest ms)))]))
  (define sorted (sort ns <))
  (go sorted (rest sorted)))

(define (solve n)
  (define seq (find-3-arithmetic-sequence (prime-permutations (get-prime n))))
  (define skip-answer (list 1487 4817 8147))
  (if (and (not (null? seq))
           (not (equal? seq skip-answer))
           (= 3 (count (lambda (ns) (= 4 (num-digits ns))) seq)))
      seq
      (solve (+ n 1))))

(display "Answer: ")
(display (foldl (lambda (n acc) (string-join (list acc (number->string n)) ""))
                ""
                (solve 1)))

