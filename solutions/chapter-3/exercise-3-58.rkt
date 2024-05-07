
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.58.
;---
;Give an interpretation of the stream computed by the following procedure:
;---
;(define (expand num den radix)
;  (cons-stream
;   (quotient (* num radix) den)
;   (expand (remainder (* num radix) den) den radix)))
;---
;('quotient' is a primitive that returns the integer quotient of two integers.) What are
;the successive elements produced by (expand 1 7 10)? What is produced by (expand 3 8 10)?
;------------------------------------------------------------------------------------------

;'expand'
;[at every step n, it computes the nth fractional part resulting from the 'radix' based
;division of 'num' by 'den', assuming num > den]
;---
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

;What are the successive elements produced by (expand 1 7 10)?
;---
;(1 7 10) -> 1
;(3 7 10) -> 4
;(2 7 10) -> 2
;(6 7 10) -> 8
;(4 7 10) -> 5
;(5 7 10) -> 7
;(1 7 10) -> 1
;(3 7 10) -> 4
;and so on...
;---
;hence, 1/7 = 0.14285714...

;What is produced by (expand 3 8 10)?
;---
;(3 8 10) -> 3
;(6 8 10) -> 7
;(4 8 10) -> 5
;(0 8 10) -> 0
;(0 8 10) -> 0
;(0 8 10) -> 0
;and so on...
;---
;hence, 3/8 = 0.375000...

