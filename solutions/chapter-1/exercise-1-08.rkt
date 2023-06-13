
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 1.8.
;---
;Newton's method for cube roots is based on the fact that if 'y' is an approximation to
;the cube root of 'x', then a better approximation is given by the value:
;(x/y^2 + 2*y) / 3
;Use this formula to implement a cube-root procedure analogous to the square-root
;procedure.
;------------------------------------------------------------------------------------------

;implementation of 'cube-root'. helper procedures are defined inside 'cube-root'. a given
;guess is considered to be good enough if abs((improve(guess)-guess)/guess) < tolerance.
;tolerance is assumed to be 0.00001 and improve is defined as (x/y^2 + 2*y) / 3
;---
(define (cube-root x)
  (define square (lambda (x) (* x x)))
  (define (improve g)
    (/ (+ (/ x (square g)) (* 2 g)) 3))
  (define (good-enuf? g)
    (define tolerance 0.00001)
    (< (abs (/ (- (improve g) g) g)) tolerance))
  (define (cube-root-iter g)
    (if (good-enuf? g)
        g
        (cube-root-iter (improve g))))
  (cube-root-iter 1.0))

;test 'cube-root' for random numbers
;---
(define (test-cube-root x)
  (display "cube-root(") (display x) (display ") = ")
  (display (cube-root x)) (newline))
;---
(test-cube-root 1)
(test-cube-root 8)
(test-cube-root 27)
(test-cube-root 64)

