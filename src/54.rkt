#lang racket

(require racket/struct)

(struct Card (value suite)
        #:methods gen:custom-write
        [(define write-proc
         (make-constructor-style-printer
           (lambda (_) 'Card)
           (lambda (obj) (list (Card-value obj) (Card-suite obj)))))])

(define (card>? a b)
  (> (Card-value a) (Card-value b)))

(define (straight? hand)
  (define (check value h)
    (cond [(null? h) #t]
          [(= value 14)
           (or ; Check if it's a straight counting the ace as 14
               (and (= (Card-value (first h)) (- value 1))
                    (check (- value 1) (rest h)))
               ; Check if it's a straight counting the ace as 1
               (check (Card-value (first h)) (append (rest h) (list (Card 1 #\?)))))]
          [(= (Card-value (first h)) (- value 1)) (check (- value 1) (rest h))]
          [else #f]))
    (check (Card-value (first hand)) (rest hand)))

(define (flush? hand)
  (define (check suite h)
    (cond [(null? h) #t]
          [(equal? suite (Card-suite (first h))) (check suite (rest h))]
          [else #f]))
  (check (Card-suite (first hand)) (rest hand)))

; Returns (list (cons n-of-a-kind value)) ordered from highest n-of-a-kind, then highest value
(define (of-a-kindedness hand)
  (define v (make-vector 15 0))
  (for-each (lambda (c)
                    (define old-value (vector-ref v (Card-value c)))
                    (vector-set! v (Card-value c) (+ 1 old-value)))
            hand)
  (reverse (sort (filter (lambda (p) (not (= 0 (car p))))
                         (map cons (vector->list v) (range 0 15)))
                 (lambda (a b) (cond [(< (car a) (car b)) #t]
                                     [(< (cdr a) (cdr b)) #t]
                                     [else #f])))))

; Returns a list of numbers to compare hands, expects sorted hands
; First number will be the hand rating:
; 1 = high card
; 2 = one pair
; 3 = two pair
; 4 = three of a kind
; 5 = straight
; 6 = flush
; 7 = full house
; 8 = four of a kind
; 9 = straight flush
; The next numbers will be the highest card used in the hand, then the next highest, etc. for breaking ties
(define (hand-score hand)
  (define is-straight? (straight? hand))
  (define is-flush? (flush? hand))
  (define kindedness (of-a-kindedness hand))
  (define max-of-a-kind (car (first kindedness)))
  (define second-of-a-kind (car (second kindedness)))
  (define tiebreakers (cond ; Need to use (rest hand) here, if I didn't, then A5432 would be incorrectly scored higher than 65432
                            [is-straight? (rest hand)]
                            [else (map cdr kindedness)]))
  (define score (cond [(and is-flush? is-straight?) 9]
                      [(= 4 max-of-a-kind) 8]
                      [(and (= 3 max-of-a-kind) (= 2 second-of-a-kind)) 7]
                      [is-flush? 6]
                      [is-straight? 5]
                      [(= 3 max-of-a-kind) 4]
                      [(and (= 2 max-of-a-kind) (= 2 second-of-a-kind)) 3]
                      [(= 2 max-of-a-kind) 2]
                      [else 1]))
  (cons score tiebreakers))

; Compares values, expects sorted hands
(define (hand>? a b)
  (define score-a (hand-score a))
  (define score-b (hand-score b))
  (define (go sa sb)
    (cond [(= (first sa) (first sb)) (go (rest sa) (rest sb))]
          [(> (first sa) (first sb)) #t]
          [else #f]))
  (go score-a score-b))

(define (string->card s)
  (define value (match (string-ref s 0)
                       [#\2 2]
                       [#\3 3]
                       [#\4 4]
                       [#\5 5]
                       [#\6 6]
                       [#\7 7]
                       [#\8 8]
                       [#\9 9]
                       [#\T 10]
                       [#\J 11]
                       [#\Q 12]
                       [#\K 13]
                       [#\A 14]))
  (define suite (string-ref s 1))
  (Card value suite))

(define (string->hand s)
  (sort (map string->card (string-split s " ")) card>?))

(define input
  (map (lambda (s)
               (define s1 (make-string 15 #\_))
               (define s2 (make-string 14 #\_))
               (string-copy! s1 0 s 0 15)
               (string-copy! s2 0 s 15)
               (cons (string->hand s1) (string->hand s2)))
       (file->lines "data/0054_poker.txt")))


(foldl (lambda (p acc) (if (hand>? (car p) (cdr p))
                           (+ acc 1)
                           acc))
       0 input)
