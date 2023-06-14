
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.46.
;---
;A two-dimensional vector 'v' running from the origin to a point can be represented as a
;pair consisting of an x-coordinate and a y-coordinate. Implement a data abstraction for
;vectors by giving a constructor 'make-vect' and corresponding selectors 'xcor-vect' and
;'ycor-vect'. In terms of your selectors and constructor, implement procedures
;'add-vect', 'sub-vect', and 'scale-vect' that perform the operations vector addition,
;vector subtraction, and multiplying a vector by a scalar:
;---
;vector addition -> (x1, y1) + (x2, y2) = (x1+x2, y1+y2)
;vector subtraction -> (x1, y1) - (x2, y2) = (x1-x2, y1-y2)
;vector scaling -> s * (x, y) = (s*x, s*y)
;------------------------------------------------------------------------------------------

;constructor -> 'make-vect'
;---
(define (make-vect x y) (cons x y))

;selectors -> 'xcor-vect' and 'ycor-vect'
;---
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

;'add-vect'
;---
(define (add-vect v w)
  (make-vect (+ (xcor-vect v) (xcor-vect w))
             (+ (ycor-vect v) (ycor-vect w))))

;'sub-vect'
;---
(define (sub-vect v w)
  (make-vect (- (xcor-vect v) (xcor-vect w))
             (- (ycor-vect v) (ycor-vect w))))

;'scale-vect'
;---
(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

;test for random vectors
;---
(define v (make-vect 1 2))
(define w (make-vect 3 4))
(display "v -> ") v
(display "w -> ") w
(display "(add-vect v w) -> ") (add-vect v w)
(display "(sub-vect v w) -> ") (sub-vect v w)
(display "(scale 10 v) -> ") (scale-vect 10 v)

