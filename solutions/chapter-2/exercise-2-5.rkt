
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.5.
;---
;Show that we can represent pairs of nonnegative integers using only numbers and
;arithmetic operations if we represent the pair 'a' and 'b' as the integer that is the
;product 2^a * 3^b. Give the corresponding definitions of the procedures 'cons', 'car',
;and 'cdr'.
;------------------------------------------------------------------------------------------

;implementing a procedure for performing fast exponentiation
;suppose we have a base b and an exponent e. to get b^e, we can do the following:
;- if e = 0, return 1
;- if e is odd, then b^e = b * b^(e-1)
;- if e is even, then b^e = (b^(e/2))^2
;---
(define square (lambda (x) (* x x)))
(define even? (lambda (x) (= (remainder x 2) 0)))
(define (fast-exp b e)
  (cond ((= e 0) 1)
        ((even? e) (square (fast-exp b (/ e 2))))
        (else (* b (fast-exp b (dec e))))))

;defining the constructor ('cons')
;---
(define (cons-alt x y)
  (* (fast-exp 2 x) (fast-exp 3 y)))

;rationale
;---
;to implement 'car', iteratively divide the result of 'cons' by 2 until the remainder
;of result and 2 is non-zero (while result % 2 == 0: result //= 2)
;similar reasoning for the implementation of 'cdr' (while result % 3 == 0: result //= 3)

;helper procedure that allows to compute the number of times it is possible to divide a
;given number 'num' by a base 'base' without remainder
;---
(define (num-divs num base)
  (define (iter temp count)
    (if (= (remainder temp base) 0)
        (iter (/ temp base) (inc count))
        count))
  (iter num 0))

;defining the selectors ('car' and 'cdr')
;---
(define (car-alt p) (num-divs p 2))
(define (cdr-alt p) (num-divs p 3))

;test for random pairs
;---
(define (test x y)
  (let ((p (cons-alt x y)))
    (display "cons = ") (display p) (newline)
    (display "car = ") (display (car-alt p)) (newline)
    (display "cdr = ") (display (cdr-alt p)) (newline)))
;---
(test 3 7)
(display "---") (newline)
(test 34 87)

