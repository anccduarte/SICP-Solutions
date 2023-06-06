
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.34.
;---
;Evaluating a polynomial in 'x' at a given value of 'x' can be formulated as an
;accumulation. We evaluate the polynomial
;---
;a(n)*x^n + a(n-1)*x^(n-1) + ... + a(1)*x + a(0)
;---
;using a well-known algorithm called Horner's rule, which structures the computation as
;---
;(... (a(n)*x + a(n-1))*x + ... + a(1))*x + a(0).
;---
;In other words, we start with a(n), multiply by 'x', add a(n-1), multiply by 'x', and so
;on, until we reach a(0). Fill in the following template to produce a procedure that
;evaluates a polynomial using Horner's rule. Assume that the coefficients of the
;polynomial are arranged in a sequence, from a(0) through a(n).
;---
;(define (horner-eval x coefficient-sequence)
;  (accumulate (lambda (this-coeff higher-terms) <??>)
;              0
;              coefficient-sequence))
;---
;For example, to compute 1 + 3x + 5x^3 + x^5 at x=2 you would evaluate
;---
;(horner-eval 2 (list 1 3 0 5 0 1))
;------------------------------------------------------------------------------------------

;'horner' -> not in terms of 'accumulate'
;---
;consider the example stated above:
;1 + 3x + 5x^3 + x^5 = 1 + 3x + 0x^2 + 5x^3 + 0x^4 + x^5
;by horner's rule: (((((1*x + 0)*x + 5)*x + 0)*x + 3)*x + 1)
;hence, (+ (* (+ (* (+ (* (+ (* (+ (* 1 x) 0) x) 5) x) 0) x) 3) x) 1)
;this results in a very straightforward recursive implementation of the rule
;---
(define (horner x coeffs)
  (if (null? coeffs)
      0
      (+ (car coeffs)
         (* x
            (horner x (cdr coeffs))))))

;'accumulate' -> helper
;---
(define (accumulate op initial lst)
  (if (null? lst)
      initial
      (op (car lst)
          (accumulate op initial (cdr lst)))))

;'horner-eval' -> in terms of 'accumulate'
;---
;uncovering the structure of the problem in terms of simple recursion, makes it easier to
;reimplement the horner's rule in terms of 'accumulate'
;---
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff
                   (* x
                      higher-terms)))
              0
              coefficient-sequence))

;considering the example stated above, and assuming that 'p' is the procedure passed to
;'accumulate', the process evolves the following way
;---
;(horner-eval 2 (list 1 3 0 5 0 1))
;---
;(accumulate p 0 (list 1 3 0 5 0 1))
;---
;(+ 1
;   (* 2 (accumulate p 0 (list 3 0 5 0 1))))
;---
;(+ 1
;   (* 2 (+ 3
;           (* 2 (accumulate p 0 (list 0 5 0 1))))))
;---
;(+ 1
;   (* 2 (+ 3
;           (* 2 (+ 0
;                   (* 2 (accumulate p 0 (list 5 0 1))))))))
;---
;(+ 1
;   (* 2 (+ 3
;           (* 2 (+ 0
;                   (* 2 (+ 5
;                           (* 2 (accumulate p 0 (list 0 1))))))))))
;---
;(+ 1
;   (* 2 (+ 3
;           (* 2 (+ 0
;                   (* 2 (+ 5
;                           (* 2 (+ 0
;                                   (* 2 (accumulate p 0 (list 1))))))))))))
;---
;(+ 1
;   (* 2 (+ 3
;           (* 2 (+ 0
;                   (* 2 (+ 5
;                           (* 2 (+ 0
;                                   (* 2 (+ 1
;                                           (* 2 (accumulate p 0 nil)))))))))))))
;---
;(+ 1
;   (* 2 (+ 3
;           (* 2 (+ 0
;                   (* 2 (+ 5
;                           (* 2 (+ 0
;                                   (* 2 (+ 1
;                                           (* 2 0))))))))))))
;---
;79

;test both implementations
;---
(define (test x coeffs)
  (display "x = ") (display x) (newline)
  (display "coeffs = ") (display coeffs) (newline)
  (display "(horner x coeffs) -> ") (display (horner x coeffs)) (newline)
  (display "(horner-eval x coeffs) -> ") (display (horner-eval x coeffs)) (newline))
;---
(define coeffs (list 1 3 0 5 0 1))
(test 2 coeffs)
