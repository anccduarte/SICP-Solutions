
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 1.46.
;---
;Several of the numerical methods described in this chapter are instances of an extremely
;general computational strategy known as iterative improvement. Iterative improvement
;says that, to compute something, we start with an initial guess for the answer, test if
;the guess is good enough, and otherwise improve the guess and continue the process using
;the improved guess as the new guess. Write a procedure 'iterative-improve' that takes
;two procedures as arguments: a method for telling whether a guess is good enough and a
;method for improving a guess. 'iterative-improve' should return as its value a procedure
;that takes a guess as argument and keeps improving the guess until it is good enough.
;Rewrite the 'sqrt' procedure of Section 1.1.7 and the 'fixed-point' procedure of Section
;1.3.3 in terms of 'iterative-improve'.
;------------------------------------------------------------------------------------------

;defining 'iterative-improve'
;---
(define (iterative-improve good-enough? improve)
  (define (iter guess)
      (let ((next (improve guess)))
        (if (good-enough? guess next)
            next
            (iter next))))
  iter)

;defining 'fixed-point' in terms of 'iterative-improve'
;---
(define (fixed-point f guess)
  (define (close-enuf? old new)
    (let ((tolerance 0.0001))
      (< (abs (- new old)) tolerance)))
  ((iterative-improve close-enuf? f) guess))

;defining 'sqrt' in terms of the new 'fixed-point' procedure
;---
(define (sqrt x)
  (define (average-damp f)
    (lambda (x) (/ (+ x (f x)) 2)))
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

;test 'sqrt' for some perfect squares
;---
(define (test x)
  (display "sqrt(") (display x) (display ") = ")
  (display (sqrt x)) (newline))
;---
(test 4)
(test 9)
(test 16)
(test 25)

