
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.3.
;---
;Implement a representation for rectangles in a plane. (Hint: You may want to make use of
;Exercise 2.2.) In terms of your constructors and selectors, create procedures that
;compute the perimeter and the area of a given rectangle. Now implement a different
;representation for rectangles. Can you design your system with suitable abstraction
;barriers, so that the same 'perimeter' and 'area' procedures will work using either
;representation?
;------------------------------------------------------------------------------------------

;some previously implemented procedures (point and segment)
;---
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
;---
(define (make-segment p q) (cons p q))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

;CONSTRUCTOR: make-rectangle
;SELECTORS: width-rectangle, height-rectangle

;1st REPRESENTATION
;rectangle in terms of segments
;from one segment, we take the width of the rectangle. from the other segment, we take
;the height of the rectangle
;condition: to produce a rectangle, the segments must be orthogonal (two segments are
;orthogonal if the dot product of the vectors representing them is zero)
;---
;transform segment in vector (to perform dot product)
(define (to-vector s)
  (let ((start (start-segment s))
        (end (end-segment s)))
    (let ((x-start (x-point start))
          (x-end (x-point end))
          (y-start (y-point start))
          (y-end (y-point end)))
      (make-point (- x-end x-start)
                  (- y-end y-start)))))
;---
;define 'dot-product' (helper)
(define (dot-product v1 v2)
  (let ((x1 (x-point v1))
        (y1 (y-point v1))
        (x2 (x-point v2))
        (y2 (y-point v2)))
    (+ (* x1 x2) (* y1 y2))))
;---
;check orthogonality of two segments
(define (orthogonal? s1 s2)
  (let ((v1 (to-vector s1))
        (v2 (to-vector s2)))
    (= (dot-product v1 v2) 0)))
;---
;construct rectangle from two segments
;s1 is assumed to be the width and s2 to be the height
(define (make-rectangle-v1 s1 s2)
  (if (orthogonal? s1 s2)
      (cons s1 s2)
      (error "Cannot construct a rectangle (the segments are not orthogonal)")))
;---
;before implementing the selectors, we have to have a way to compute the size of a
;segment -> pythagoras
(define square (lambda (x) (* x x)))
(define (size s)
  (let ((start (start-segment s))
        (end (end-segment s)))
    (let ((x-start (x-point start))
          (x-end (x-point end))
          (y-start (y-point start))
          (y-end (y-point end)))
      (sqrt (+ (square (- x-end x-start))
               (square (- y-end y-start)))))))
;---
;selectors for width and height
(define (width-rectangle-v1 r) (size (car r)))
(define (height-rectangle-v1 r) (size (cdr r)))
;---
;test 1st representation for random rectangle
(let ((s1 (make-segment (make-point 0 0) (make-point 2 0)))
      (s2 (make-segment (make-point 0 0) (make-point 0 4))))
  (let ((rectangle (make-rectangle-v1 s1 s2)))
    (display "width = ") (display (width-rectangle-v1 rectangle))
    (newline)
    (display "height = ") (display (height-rectangle-v1 rectangle))
    (newline)))

;2nd REPRESENTATION
;rectangle in terms of width, height, origin and angle
;idea from https://sicp-solutions.net/post/sicp-solution-exercise-2-3/
;'width' and 'height' -> self explanatory
;'origin' -> coordinates of the bottom left corner of the rectangle
;'angle' -> angle between an hypothetical segment produced by 'width' and an horizontal
;           line crossing 'origin'
;---
;pair of pairs -> ((width, height), (origin, angle))
(define (make-rectangle-v2 width height origin angle)
  (cons (cons width height) (cons origin angle)))
;---
;selectors for 'width' and 'height'
(define (width-rectangle-v2 r) (car (car r)))
(define (height-rectangle-v2 r) (cdr (car r)))
;---
;selectors for 'origin' and 'angle'
(define (origin-rectangle r) (car (cdr r)))
(define (angle-rectangle r) (cdr (cdr r)))
;---
;test 2st representation for random rectangle
(let ((rectangle (make-rectangle-v2 2 4 (make-point 0 0) 45)))
  (display "width = ") (display (width-rectangle-v2 rectangle))
  (newline)
  (display "height = ") (display (height-rectangle-v2 rectangle))
  (newline))

;IMPLEMENTATION OF PERIMETER AND AREA (using both representations)
;---
;1st REPRESENTATION
;---
;perimeter
(define (perimeter-v1 r)
  (+ (* 2 (width-rectangle-v1 r))
     (* 2 (height-rectangle-v1 r))))
;area
(define (area-v1 r)
  (* (width-rectangle-v1 r) (height-rectangle-v1 r)))
;---
;test for random rectangle
(let ((s1 (make-segment (make-point 0 0) (make-point 2 0)))
      (s2 (make-segment (make-point 0 0) (make-point 0 4))))
  (let ((rectangle (make-rectangle-v1 s1 s2)))
    (display "perimeter-v1 = ") (display (perimeter-v1 rectangle))
    (newline)
    (display "area-v1 = ") (display (area-v1 rectangle))
    (newline)))
;---
;2nd REPRESENTATION
;---
;perimeter (same as before - only the selection of width and height is distinct)
(define (perimeter-v2 r)
  (+ (* 2 (width-rectangle-v2 r))
     (* 2 (height-rectangle-v2 r))))
;area (same as before - only the selection of width and height is distinct)
(define (area-v2 r)
  (* (width-rectangle-v2 r) (height-rectangle-v2 r)))
;---
;test for random rectangle
(let ((rectangle (make-rectangle-v2 2 4 (make-point 0 0) 45)))
  (display "perimeter-v2 = ") (display (perimeter-v2 rectangle))
  (newline)
  (display "area-v2 = ") (display (area-v2 rectangle))
  (newline))

