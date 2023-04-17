
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.2.
;---
;Consider the problem of representing line segments in a plane. Each segment is
;represented as a pair of points: a starting point and an ending point. Define a
;constructor 'make-segment' and selectors 'start-segment' and 'end-segment' that define
;the representation of segments in terms of points. Furthermore, a point can be
;represented as a pair of numbers: the x coordinate and the y coordinate. Accordingly,
;specify a constructor 'make-point' and selectors 'x-point' and 'y-point' that define
;this representation. Finally, using your selectors and constructors, define a procedure
;'midpoint-segment' that takes a line segment as argument and returns its midpoint (the
;point whose coordinates are the average of the coordinates of the endpoints).
;------------------------------------------------------------------------------------------

;point -> pair of numbers
;         constructor: make-point
;         selectors: x-point, y-point
;---
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

;pretty print points
;---
(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")")
  (newline))

;segment -> pair of points
;           constructor: make-segment
;           selectors: start-segment, end-segment
;---
(define (make-segment p q) (cons p q))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

;midpoint of a segment:
;mid = ((x-point(start-segment) + x-point(end-segment)) / 2,
;       (y-point(start-segment) + y-point(end-segment)) / 2)
;---
(define (midpoint-segment s)
  (let ((start (start-segment s))
        (end (end-segment s)))
    (make-point (/ (+ (x-point start) (x-point end)) 2.0)
                (/ (+ (y-point start) (y-point end)) 2.0))))

;test for random segment
;---
(let ((p1 (make-point 2 4))
      (p2 (make-point -1 5)))
  (let ((s (make-segment p1 p2)))
    (display "midpoint = ")
    (print-point (midpoint-segment s))))

