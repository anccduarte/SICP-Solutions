
#lang sicp
(#%require sicp-pict)

;------------------------------------------------------------------------------------------
;EXERCISE 2.51.
;---
;Define the 'below' operation for painters. 'below' takes two painters as arguments. The
;resulting painter, given a frame, draws with the first painter in the bottom of the
;frame and with the second painter in the top. Define 'below' in two different ways —
;first by writing a procedure that is analogous to the 'beside' procedure given above,
;and again in terms of 'beside' and suitable rotation operations (from Exercise 2.50).
;------------------------------------------------------------------------------------------

;the following procedures (available in 'sicp-pict') are used for convenience:
;---
;- 'make-vect', 'vector-xcor', 'vector-ycor'
;- 'vector-add', 'vector-sub', 'vector-scale'
;- 'make-frame', 'frame-origin', 'frame-edge1', 'frame-edge2'
;- 'rotate180', 'rotate270'

;'frame-coord-map'
;---
(define (frame-coord-map frame)
  (lambda (vec)
    (vector-add (frame-origin frame)
                (vector-add (vector-scale (vector-xcor vec)
                                          (frame-edge1 frame))
                            (vector-scale (vector-ycor vec)
                                          (frame-edge2 frame))))))

;'transform-frame'
;---
(define (transform-frame origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (make-frame new-origin
                    (vector-sub (m corner1) new-origin)
                    (vector-sub (m corner2) new-origin))))))

;'beside'
;(here, 'beside' is implemented with an extra argument 's'; 's' represents the x
;coordinate at which the frame is split)
;---
(define (beside painter1 painter2 s)
  (lambda (frame)
    (let ((lt (transform-frame (make-vect 0.0 0.0)
                               (make-vect s 0.0)
                               (make-vect 0.0 1.0)))
          (rt (transform-frame (make-vect s 0.0)
                               (make-vect 1.0 0.0)
                               (make-vect s 1.0))))
      (painter1 (lt frame))
      (painter2 (rt frame)))))

;'below-v1'
;(similar implementation to 'beside')
;---
(define (below-v1 painter1 painter2 s)
  (lambda (frame)
    (let ((bt (transform-frame (make-vect 0.0 0.0)
                               (make-vect 1.0 0.0)
                               (make-vect 0.0 s)))
          (tt (transform-frame (make-vect 0.0 s)
                               (make-vect 1.0 s)
                               (make-vect 0.0 1.0))))
      (painter1 (bt frame))
      (painter2 (tt frame)))))

;'below-v2'
;('below' in terms of 'beside' and suitable rotation operations; very similar to
;'below-v1': probably not the way to go)
;---
(define (below-v2 painter1 painter2 s)
  (lambda (frame)
    (let ((bt (transform-frame (make-vect 0.0 0.0)
                               (make-vect 1.0 0.0)
                               (make-vect 0.0 s)))
          (tt (transform-frame (make-vect 0.0 s)
                               (make-vect 1.0 s)
                               (make-vect 0.0 1.0))))
      (beside (painter1 (bt frame))
              (painter2 (tt frame))
              s))))

;'below-v3'
;(much smarter and less intricate than the previous version -> in terms of rotators)
;---
(define (below-v3 painter1 painter2 s)
  (rotate90 (beside (rotate270 painter1)
                    (rotate270 painter2)
                    s)))

;test all implementations of 'below'
;---
(define (for-each proc lst)
  (cond ((null? lst)
         (display ""))
        (else
         (proc (car lst))
         (for-each proc (cdr lst)))))
;---
(define (test proc)
  (display proc) (newline)
  (for-each (lambda (x)
              (display (paint (proc einstein einstein x))))
            (list 0.5 0.75 1.0))
  (newline) (newline))
;---
(test below-v1)
(test below-v2)
(test below-v3)

