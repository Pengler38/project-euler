#lang racket

; Returns empty if ns isn't long enough to take n
(define (safetake ns n)
  (with-handlers ([exn:fail:contract?
                  (lambda (_) empty)])
                 (take ns n)
                 ))

(define (solve input n)
  (define (go in max)
    (if (empty? in)
        max
        (let* ([ints (safetake in n)]
               [product (foldl * 1 ints)])
          (if (< max product)
              (go (cdr in) product)
              (go (cdr in) max)
              ))))
  (go input 0)
)

(let* ([input (car (file->lines "src/8.txt"))]
       [int-list (map (lambda (c) (string->number (string c))) (string->list input))])
  (display "Input: ")
  (displayln int-list)

  (display "Testing: ")
  (println (solve int-list 4))

  (display "Answer: ")
  (println (solve int-list 13))
  )


