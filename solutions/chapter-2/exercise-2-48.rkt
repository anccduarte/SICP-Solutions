
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.48.
;---
;A directed line segment in the plane can be represented as a pair of vectors — the
;vector running from the origin to the start-point of the segment, and the vector running
;from the origin to the end-point of the segment. Use your vector representation from
;Exercise 2.46 to define a representation for segments with a constructor
;'make-segment' and selectors 'start-segment' and 'end-segment'.
;------------------------------------------------------------------------------------------

;constructor -> 'make-segment'
;---
(define (make-segment v w) (cons v w))

;selectors -> 'start-segment' and 'end-segment'
;---
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

;test for random vectors
;---
(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))
;---
(define v (make-vect 1 2))
(define w (make-vect 3 4))
;---
(define seg (make-segment v w))
(display "seg -> ") seg
(display "(start-segment seg) -> ") (start-segment seg)
(display "(end-segment seg) -> ") (end-segment seg)

