
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.1.
;---
;An accumulator is a procedure that is called repeatedly with a single numeric argument
;and accumulates its arguments into a sum. Each time it is called, it returns the
;currently accumulated sum. Write a procedure 'make-accumulator' that generates
;accumulators, each maintaining an independent sum. The input to 'make-accumulator'
;should specify the initial value of the sum; for example
;---
;(define A (make-accumulator 5))
;(A 10) -> 15
;(A 10) -> 25
;------------------------------------------------------------------------------------------

;'make-accumulator'
;---
(define (make-accumulator total)
  (lambda (val)
    (set! total (+ total val))
    total))

;test 'make-accumulator'
;---
(define A1 (make-accumulator 10))
(A1 2) (A1 -1) (A1 5)
;---
(display "---") (newline)
(define A2 (make-accumulator 5))
(A2 6) (A2 -2) (A2 10)

