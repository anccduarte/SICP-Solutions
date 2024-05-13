
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.72.
;---
;In a similar way to Exercise 3.71 generate a stream of all numbers that can be written
;as the sum of two squares in three different ways (showing how they can be so written).
;------------------------------------------------------------------------------------------

;auxiliary
;---
(define square (lambda (x) (* x x)))
(define sum-squares (lambda (p) (+ (square (car p)) (square (cadr p)))))
;---
(define (move n s)
  (if (= n 0)
      s
      (move (- n 1) (stream-cdr s))))

;'3-way-sum-squares'
;[see note in 'ramanujan-numbers-fail' at 3.71 for implementation details]
;---
(define (3-way-sum-squares)
  (let ((sum-squares-sorted
         (weighted-pairs integers integers sum-squares)))
    (let lp ((s sum-squares-sorted))
      (let ((p1 (stream-car s))
            (p2 (stream-car (move 1 s)))
            (p3 (stream-car (move 2 s)))
            (p4 (stream-car (move 3 s))))
        (if (= (sum-squares p1) (sum-squares p2) (sum-squares p3))
            (if (not (= (sum-squares p1) (sum-squares p4)))
                (cons-stream (list p1 p2 p3 (sum-squares p1))
                             (lp (move 3 s)))                 ;ignore first 3 pairs
                (lp (move 4 s)))                              ;ignore first 4 pairs
            (lp (move 1 s)))))))                              ;move to next pair in 's'

