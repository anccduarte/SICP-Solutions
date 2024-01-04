
#lang sicp


;------------------------------------------------------------------------------------------
;EXTENDED EXERCISE: RATIONAL FUNCTIONS (2.93. to 2.97.)
;------------------------------------------------------------------------------------------


;------------------------------------------------------------------------------------------
;EXERCISE 2.93.
;---
;Modify the rational-arithmetic package to use generic operations, but change 'make-rat'
;so that it does not attempt to reduce fractions to lowest terms. Test your system by
;calling 'make-rational' on two polynomials to produce a rational function:
;---
;(define p1 (make-polynomial 'x '((2 1) (0 1))))
;(define p2 (make-polynomial 'x '((3 1) (0 1))))
;(define rf (make-rational p2 p1))
;---
;Now add 'rf' to itself, using add. You will observe that this addition procedure does
;not reduce fractions to lowest terms.
;------------------------------------------------------------------------------------------

;TODO
;---
;To be solved on a second reading of the textbook. These exercises are concerned with the
;implementation of the generic arithmetic package. Given the extended period of time in
;which reading and solving SICP exercises were left aside, such undertaking would become
;quite cumbersome.
;---
(display "Exercise 2.93.") (newline)
(display "---") (newline)


;------------------------------------------------------------------------------------------
;EXERCISE 2.94.
;---
;Using 'div-terms', implement the procedure 'remainder-terms' and use this to define
;'gcd-terms' as above. Now write a procedure 'gcd-poly' that computes the polynomial GCD
;of two polys. (The procedure should signal an error if the two polys are not in the same
;variable.) Install in the system a generic operation 'greatest-common-divisor' that
;reduces to 'gcd-poly' for polynomials and to ordinary 'gcd' for ordinary numbers. As a
;test, try
;---
;(define p1 (make-polynomial
;            'x '((4 1) (3 -1) (2 -2) (1 2))))
;(define p2 (make-polynomial 'x '((3 1) (1 -1))))
;(greatest-common-divisor p1 p2)
;---
;and check your result by hand.
;------------------------------------------------------------------------------------------

;TODO
;---
;To be solved on a second reading of the textbook. These exercises are concerned with the
;implementation of the generic arithmetic package. Given the extended period of time in
;which reading and solving SICP exercises were left aside, such undertaking would become
;quite cumbersome.
;---
(display "Exercise 2.94.") (newline)
(display "---") (newline)


;------------------------------------------------------------------------------------------
;EXERCISE 2.95.
;---
;Define P1, P2, and P3 to be the polynomials
;---
;P1 : x^2 - 2x + 1
;P2 : 11x^2 + 7
;P3 : 13x + 5
;---
;Now define Q1 to be the product of P1 and P2 and Q2 to be the product of P1 and P3, and
;use 'greatest-common-divisor' (Exercise 2.94) to compute the GCD of Q1 and Q2. Note that
;the answer is not the same as P1. This example introduces noninteger operations into the
;computation, causing difficulties with the GCD algorithm. To understand what is
;happening, try tracing 'gcd-terms' while computing the GCD or try performing the
;division by hand.
;------------------------------------------------------------------------------------------

;TODO
;---
;To be solved on a second reading of the textbook. These exercises are concerned with the
;implementation of the generic arithmetic package. Given the extended period of time in
;which reading and solving SICP exercises were left aside, such undertaking would become
;quite cumbersome.
;---
(display "Exercise 2.95.") (newline)
(display "---") (newline)


;------------------------------------------------------------------------------------------
;EXERCISE 2.96.
;---
;(a) Implement the procedure 'pseudoremainder-terms', which is just like
;'remainder-terms' except that it multiplies the dividend by the integerizing factor
;described above before calling 'div-terms'. Modify 'gcd-terms' to use
;'pseudoremainder-terms', and verify that 'greatest-common-divisor' now produces an
;answer with integer coefficients on the example in Exercise 2.95.
;---
;(b) The GCD now has integer coefficients, but they are larger than those of P1. Modify
;'gcd-terms' so that it removes common factors from the coefficients of the answer by
;dividing all the coefficients by their (integer) greatest common divisor.
;------------------------------------------------------------------------------------------

;TODO
;---
;To be solved on a second reading of the textbook. These exercises are concerned with the
;implementation of the generic arithmetic package. Given the extended period of time in
;which reading and solving SICP exercises were left aside, such undertaking would become
;quite cumbersome.
;---
(display "Exercise 2.96.") (newline)
(display "---") (newline)


;------------------------------------------------------------------------------------------
;EXERCISE 2.97.
;---
;(a) Implement this algorithm [see SICP] as a procedure 'reduce-terms' that takes two
;term lists 'n' and 'd' as arguments and returns a list 'nn', 'dd', which are 'n' and 'd'
;reduced to lowest terms via the algorithm given above. Also write a procedure
;'reduce-poly', analogous to 'add-poly', that checks to see if the two polys have the
;same variable. If so, 'reduce-poly' strips off the variable and passes the problem to
;'reduce-terms', then reattaches the variable to the two term lists supplied by
;'reduce-terms'.
;---
;(b) Define a procedure analogous to 'reduce-terms' that does what the original
;'make-rat' did for integers:
;---
;(define (reduce-integers n d)
;  (let ((g (gcd n d)))
;    (list (/ n g) (/ d g))))
;---
;and define 'reduce' as a generic operation that calls 'apply-generic' to dispatch to
;either 'reduce-poly' (for polynomial arguments) or 'reduce-integers' (for
;'scheme-number' arguments). You can now easily make the rational-arithmetic package
;reduce fractions to lowest terms by having 'make-rat' call 'reduce' before combining the
;given numerator and denominator to form a rational number. The system now handles
;rational expressions in either integers or polynomials. To test your program, try the
;example at the beginning of this extended exercise:
;---
;(define p1 (make-polynomial 'x '((1 1) (0 1))))
;(define p2 (make-polynomial 'x '((3 1) (0 -1))))
;(define p3 (make-polynomial 'x '((1 1))))
;(define p4 (make-polynomial 'x '((2 1) (0 -1))))
;(define rf1 (make-rational p1 p2))
;(define rf2 (make-rational p3 p4))
;(add rf1 rf2)
;---
;See if you get the correct answer, correctly reduced to lowest terms.
;------------------------------------------------------------------------------------------

;TODO
;---
;To be solved on a second reading of the textbook. These exercises are concerned with the
;implementation of the generic arithmetic package. Given the extended period of time in
;which reading and solving SICP exercises were left aside, such undertaking would become
;quite cumbersome.
;---
(display "Exercise 2.97.") (newline)
(display "---") (newline)

