#lang racket

; There must be half Rights and half Downs in a 20x20 grid
; Then it is only scrambling those moves to make the different routes

; This is equivalent to a k-combination of 2n items, choose n.
; There are two choices, Right or Down, which is exactly like how a k-combination chooses present/not-present.
; There must be n Rights/'presents', and n Downs/'not-presents' so you choose n of 2n items

; I have a feeling there should be a more general way to do this, for example if there were three directions.
; I'm not sure how to do it though.

(require "utils.rkt")

(define (solve n)
  (k-combination (* 2 n)
                 n))

(display "Test, should be 6: ")
(displayln (solve 2))
(display "Answer: ")
(displayln (solve 20))
