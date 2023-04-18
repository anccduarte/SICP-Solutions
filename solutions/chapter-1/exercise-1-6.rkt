
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 1.6.
;---
;Alyssa P. Hacker doesn't see why 'if' needs to be provided as a special form. "Why can't
;I just define it as an ordinary procedure in terms of 'cond'?" she asks. Alyssa’s friend
;Eva Lu Ator claims this can indeed be done, and she defines a new version of if (SEE
;BELOW *). Eva demonstrates the program for Alyssa. Delighted, Alyssa uses 'new-if' to
;rewrite the 'square-root' program (SEE BELOW **). What happens when Alyssa attempts to
;use this to compute square roots? Explain.
;------------------------------------------------------------------------------------------

;(*) 'new-if'
;---
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;(**) 'sqrt-iter' in terms of 'new-if'
;---
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))
;---
(define (good-enough? guess x)
  (< (abs (- x (* guess guess))) 0.00001))
;---
(define (improve guess x)
  (/ (+ guess (/ x guess)) 2))

;test 'sqrt-iter' for random numbers
;---
(define (test-sqrt x)
  (display "sqrt(") (display x) (display ") = ")
  (display (sqrt-iter 1.0 x)) (newline))
;---
;(test-sqrt 1)
;(test-sqrt 2)
;(test-sqrt 3)
;(test-sqrt 4)
;---
;since 'new-if' is a procedure rather than a special form (like 'if') and since Lisp
;follows the applicative-order for expression evaluations, all operands of the
;combination whose operator is 'new-if' will be evaluated prior to substituting the
;formal parameters of 'new-if' by the respective arguments. this results in an infinite
;loop of calls to the recursive procedure 'sqrt-iter'.

