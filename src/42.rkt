#lang racket

(require "utils.rkt")

(define input (string-split (string-replace (file->string "data/0042_words.txt") "\"" "") ","))

(define (char-to-int c)
  (- (char->integer c) 64))

(define word-values (map (lambda (s)
                                 (foldl (lambda (c acc) (+ acc (char-to-int c)))
                                        0
                                        (string->list s)))
                         input))

(define tri-stream (let ()
  (define (go i acc)
    (stream-cons acc (go (+ 1 i)
                         (+ acc i))))
  (go 2 1)))

(define tri-map (make-hash))

(define (is-tri? n)
  (if (<= (stream-first tri-stream) n)
      (let ()
        (hash-set! tri-map (stream-first tri-stream) #t)
        (set! tri-stream (stream-rest tri-stream))
        (is-tri? n))
      (hash-ref! tri-map n #f)))

(test-on-eq is-tri? 45 #t)
(test-on-eq is-tri? 46 #f)

(display "Answer: ")
(length (filter is-tri? word-values))

