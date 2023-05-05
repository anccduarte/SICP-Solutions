
#lang sicp


;------------------------------------------------------------------------------------------
;EXERCISE 1.41.
;---
;Define a procedure 'double' that takes a procedure of one argument as argument and
;returns a procedure that applies the original procedure twice. For example, if 'inc' is
;a procedure that adds 1 to its argument, then (double inc) should be a procedure that
;adds 2. What value is returned by (((double (double double)) inc) 5)?
;------------------------------------------------------------------------------------------


;---
;PART 1
;definition of the procedure 'double'
;---

;'double' -> procedure of one procedural argument 'f', returning a procedure of one
;argument 'x' which applies 'f' twice to 'x' -> (f (f x))
;---
(define (double f)
  (lambda (x) (f (f x))))


;---
;PART 2
;evaluation of (((double (double double)) inc) 5)
;---

;SUBSTITUTION MODEL
;---
;applicative-order evaluation
;1st -> evaluate operands
;2nd -> apply operator to operands

(((double (double double)) inc) 5)

;BUILD A STACK OF OPERATIONS
;(the expression to be evaluated is between square brackets)
;---
;1.
;[((double (double double)) inc) 5]
;operand -> 5
;operator -> ((double (double double)) inc)
;---
;2.
;([(double (double double)) inc] 5)
;operand -> inc
;operator -> (double (double double))
;---
;3.
;(([double (double double)] inc) 5)
;operand -> (double double)
;operator -> double
;---
;4.
;(((double [double double]) inc) 5)
;operand -> double
;operator -> double

;EVALUATE OPERATIONS IN THE STACK
;---
;evaluate 4.
(((double (lambda (x) (double (double x)))) inc) 5)
;---
;evaluate 3.
(((lambda (x) (double (double (double (double x))))) inc) 5)
;---
;evaluate 2.
((double (double (double (double inc)))) 5)
((double (double (double (lambda (x) (inc (inc x)))))) 5)
((double (double (lambda (x) (inc (inc (inc (inc x))))))) 5)
((double (lambda (x) (inc (inc (inc (inc (inc (inc (inc (inc x)))))))))) 5)
((lambda (x) (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc x))))))))))))))))) 5)
;---
;evaluate 1.
(inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 5))))))))))))))))

