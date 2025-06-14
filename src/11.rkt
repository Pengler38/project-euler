#lang racket

(struct 2darray (width vec))

; Returns the vec, width, and height from a 2darray
(define (2darray-attributes arr)
  (let* ([width (2darray-width arr)]
         [vec (2darray-vec arr)]
         [height (/ (vector-length vec) width)])
    (values vec width height)))

; Gets the appropriate location from a 2darray
(define (get 2d x y)
  (let-values ([(v width _) (2darray-attributes 2d)])
    (vector-ref v (+ x (* y width)))))

; Returns 0 if out of bounds
(define (safe-get 2d x y)
  (let-values ([(_ width height) (2darray-attributes 2d)])
    (if (and (< x width)
             (>= x 0)
             (< y height)
             (>= y 0))
        (get 2d x y)
        0)))

; Gets a 4 item row in the 2d array, given a direction
(define (get-row 2d x y dir)
  (let* ([next (lambda (x y) (case dir
                                   [(E) (list (add1 x) y)]
                                   [(S) (list x (add1 y))]
                                   [(SE) (list (add1 x) (add1 y))]
                                   [(SW) (list (sub1 x) (add1 y))] ))]
         [v0 (list x y)]
         [v1 (apply next v0)]
         [v2 (apply next v1)]
         [v3 (apply next v2)] )
    (map (lambda (l) (apply (lambda (x y) (safe-get 2d x y))
                            l))
         (list v0 v1 v2 v3))
    ))


; Finds the largest
(define (solve arr)
  (let-values ([(_ width height) (2darray-attributes arr)])
    ; Check the 4 to the right, 4 to the down, and 4 to the diagonals down-left + down-right, then recurse
    (define (go current-max x y)
      (let* ([get-row2 (lambda (dir) (get-row arr x y dir))]
             [rows (map get-row2 (list 'E 'S 'SE 'SW))]
             [row-sums (map (lambda (l) (apply * l)) rows)]
             [all-sums (cons current-max row-sums)]
             [new-max (apply max all-sums)]
             [new-x (if (>= x width)
                        0
                        (add1 x) )]
             [new-y (if (>= x width)
                        (add1 y)
                        y )]
             [done (> new-y height)]
             )
        (if done
            new-max
            (go new-max new-x new-y)
            )))
    (go 0 0 0)))

(let* ([input (file->lines "src/11.txt")]
       [width (string->number (car input))]
       [vec (list->vector (map string->number (string-split (cadr input) " ")))]
       [arr (2darray width vec)])
  (displayln (solve arr))
  )
