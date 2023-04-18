
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 1.3.
;---
;Define a procedure that takes three numbers as arguments and returns the sum of the
;squares of the two larger numbers.
;------------------------------------------------------------------------------------------

;implement procedure
;---
(define square (lambda (x) (* x x)))
(define (sum-squares-largest a b c)
  (cond ((> a b)
         (if (> b c)
             (+ (square a) (square b))
             (+ (square a) (square c))))
        (else
         (+ (square b) (square c)))))

;test for random arguments
;---
(sum-squares-largest 3 4 5)
(sum-squares-largest 3 -2 8)
(sum-squares-largest 3 -1 -4)

