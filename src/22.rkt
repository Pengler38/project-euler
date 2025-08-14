#lang racket

(require "utils.rkt")

(define input (file->string "data/0022_names.txt"))
(define processed-input (sort (string-split (string-replace input "\"" "") ",")
                              string<?))

(define (score str nth)
  (define value (apply + (map (lambda (c) (- (char->integer c) 64))
                              (string->list str))))
  (* value nth))

(test-on-eq (lambda (p) (apply score p)) `("COLIN" 938) 49714)

; Iterate through the input string, adding the score of each name
(define (go in total-score idx)
  (if (null? in)
      total-score
      (go (cdr in)
          (+ total-score (score (car in) (+ 1 idx)))
          (+ 1 idx))))

(define answer (go processed-input 0 0))

(display "Answer: ")
(displayln answer)
