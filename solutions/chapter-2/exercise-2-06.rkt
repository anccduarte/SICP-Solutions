
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.6.
;---
;In case representing pairs as procedures wasn't mind-boggling enough, consider that, in
;a language that can manipulate procedures, we can get by without numbers (at least
;insofar as nonnegative integers are concerned) by implementing 0 and the operation of
;adding 1 as (SEE BELOW). This representation is known as Church numerals, after its
;inventor, Alonzo Church, the logician who invented the λ-calculus. Define 'one' and
;'two' directly (not in terms of 'zero' and 'add-1'). (Hint: Use substitution to evaluate
;(add-1 zero)). Give a direct definition of the addition procedure '+' (not in terms of
;repeated application of 'add-1').
;------------------------------------------------------------------------------------------

;for a brief introduction to Church numerals, visit:
;https://www.cs.rice.edu/~javaplt/311/Readings/supplemental.pdf

;implementation of 'zero' and 'add-1'
;---
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;using the substitution model for (add-1 zero):
;---
;(add-1 zero)
;(lambda (f) (lambda (x) (f ((zero f) x))))
;(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
;(lambda (f) (lambda (x) (f ((lambda (x) x) x))))
;(lambda (f) (lambda (x) (f x)))

;defining 'one' and 'two' directly
;---
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

;implementing a procedure for addition (not in terms of 'add-1')
;in the implementation of 'add-1', a single function application is performed on the
;function composition (n f), where n determines the number of times the function f is
;applied. instead of applying f a single time (as in 'add-1'), we may apply it an
;arbitrary number of times
;---
(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

;test the previous procedures
;the church numerals are procedures which take a procedural argument f that is applied in
;succession to an argument x a given number of times. for example, (one (square) 2)
;results in the application of 'square' one time on the argument 2, that is (square 2).
;similarly, the expression ((two square) 2) results in (square (square 2))
;---
(define square (lambda (x) (* x x)))
(display "2^2 = ") (display ((one square) 2)) (newline)
(display "(2^2)^2 = ") (display ((two square) 2)) (newline)
(display "((2^2)^2)^2 = ") (display (((add-1 two) square) 2)) (newline)
(display "(((2^2)^2)^2)^2 = ") (display (((add two two) square) 2)) (newline)

