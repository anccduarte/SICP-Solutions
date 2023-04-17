
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.4.
;---
;Here is an alternative procedural representation of pairs (SEE BELOW). For this
;representation, verify that (car (cons x y)) yields 'x' for any objects 'x' and 'y'.
;What is the corresponding definition of 'cdr'? (Hint: To verify that this works, make
;use of the substitution model of Section 1.1.5.)
;------------------------------------------------------------------------------------------

;1st implementation of pairs (from text)
;'cons' returns a procedure of one argument; if the value 0 is passed as an argument to
;the returned procedure, then 'x' is returned ('car'); if the value 1 is, instead, passed
;as an argument, then 'y' is returned ('cdr')
;---
;define the constructor ('cons')
(define (cons-v1 x y)
  (lambda (m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else
           (error "message to 'cons' can only be '0' or '1'")))))
;---
;define the selectors ('car' and 'cdr')
(define (car-v1 p) (p 0))
(define (cdr-v1 p) (p 1))
;---
;test for random pair
(let ((p (cons-v1 1 2)))
  (display "car-v1 -> ") (display (car-v1 p)) (newline)
  (display "cdr-v1 -> ") (display (cdr-v1 p)) (newline))

;2nd implementation (referenced in the exercise)
;'cons' returns a procedure which takes an argument. however, this time, the argument of
;the returned procedure is yet another procedure. to emulate 'car', we want to pass to
;the procedure returned by 'cons' a procedure which takes two arguments and returns its
;first argument. in contrast, to get a 'cdr' representation, we pass a procedure that
;takes two arguments and returns the latter to the procedure returned by 'cons'
;---
;constructor
(define (cons-v2 x y) (lambda (m) (m x y)))
;---
;selectors
(define (car-v2 p) (p (lambda (a b) a)))
(define (cdr-v2 p) (p (lambda (a b) b)))
;---
;test for random pair
(let ((p (cons-v2 1 2)))
  (display "car-v2 -> ") (display (car-v2 p)) (newline)
  (display "cdr-v2 -> ") (display (cdr-v2 p)) (newline))

