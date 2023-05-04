
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 1.39.
;---
;A continued fraction representation of the tangent function was published in 1770 by the
;German mathematician J.H. Lambert:
;---
;tan(x) =        x
;         ---------------
;         1 -     x^2
;             -----------
;             3 -   x^2
;                 -------
;                 5 - ...
;---
;where 'x' is in radians. Define a procedure (tan-cf x k) that computes an approximation
;to the tangent function based on Lambert's formula. 'k' specifies the number of terms to
;compute, as in Exercise 1.37.
;------------------------------------------------------------------------------------------

;defining 'cont-frac' (recursive)
;---
(define (cont-frac n d k)
  (define (iter i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i)
                    (iter (inc i))))))
  (iter 1))

;'tan-cf' -> computes an approximation to tan(x) based on 'cont-frac'
;---
;di = 2i-1
;ni = x if i=1 else -x^2
;---
(define square (lambda (x) (* x x)))
;---
(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (- (square x))))
             (lambda (i) (- (* 2 i) 1.0))
             k))

;compare 'tan-cf' with the built-in 'tan' (k=1000)
;---
(define (test-tan x)
  (display "tan(") (display x) (display ") = ") (display (tan x))
  (newline)
  (display "tan-cf(") (display x) (display ") = ") (display (tan-cf x 1000))
  (newline))
;---
(test-tan 0)
(test-tan 2)
(test-tan 4)
(test-tan 6)

