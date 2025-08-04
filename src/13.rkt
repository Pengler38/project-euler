#lang racket

; This one is very easy because racket defaults to big ints lolol
(let* ([input (file->lines "data/13.txt")]
       [nums (map string->number input)]
       [answer (foldl + 0 nums)])
  (displayln answer))
