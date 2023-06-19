
#lang sicp
(#%require sicp-pict)


;------------------------------------------------------------------------------------------
;EXERCISE 2.50.
;---
;Define the transformation 'flip-horiz', which flips painters horizontally, and
;transformations that rotate painters counterclockwise by 180 degrees and 270 degrees.
;------------------------------------------------------------------------------------------


;the following procedures (available in 'sicp-pict') are used for convenience:
;---
;- 'make-vect', 'vector-xcor', 'vector-ycor'
;- 'vector-add', 'vector-sub', 'vector-scale'
;- 'make-frame', 'frame-origin', 'frame-edge1', 'frame-edge2'

;'frame-coord-map' -> helper
;---
(define (frame-coord-map frame)
  (lambda (vec)
    (vector-add (frame-origin frame)
                (vector-add (vector-scale (vector-xcor vec)
                                          (frame-edge1 frame))
                            (vector-scale (vector-ycor vec)
                                          (frame-edge2 frame))))))

;'transform-frame'
;(the strategy for "transforming painters" is slightly distinct from the textbook's;
;to make it clear that what is actually being transformed are frames, not painters, a
;procedure that exclusively transforms frames is considered)
;---
(define (transform-frame origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (make-frame new-origin
                    (vector-sub (m corner1) new-origin)
                    (vector-sub (m corner2) new-origin))))))


;------------------------------------------------------------------------------------------
;PART 1
;('flip-horiz')
;------------------------------------------------------------------------------------------

;'flip-horiz'
;(as a consequence of the previously mentioned modified strategy, it is made more clear
;that a procedure acting on painters returns itself a procedure that is applied to
;frames)
;---
(define (flip-horiz painter)
  (lambda (frame)
    (let ((t (transform-frame (make-vect 1.0 0.0)
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 1.0))))
      (painter (t frame)))))


;------------------------------------------------------------------------------------------
;PART 2
;(naive implementation of 'rotate180' and 'rotate270')
;------------------------------------------------------------------------------------------

;'rotate180' (naive)
;---
(define (rotate180 painter)
  (lambda (frame)
    (let ((t (transform-frame (make-vect 1.0 1.0)
                              (make-vect 0.0 1.0)
                              (make-vect 1.0 0.0))))
      (painter (t frame)))))

;'rotate270' (naive)
;---
(define (rotate270 painter)
  (lambda (frame)
    (let ((t (transform-frame (make-vect 0.0 1.0)
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 1.0))))
      (painter (t frame)))))


;------------------------------------------------------------------------------------------
;PART 3
;(idea inspired by dudrenov in http://community.schemewiki.org/?sicp-ex-2.50:
;define 'repeated' and 'frame-rotate-90', and implement a procedure 'rotate-n' that
;rotates a painter by n*90 degrees. 'rotate-180' and 'rotate-270' are then implemented
;in terms of 'rotate-n')
;------------------------------------------------------------------------------------------

;'repeated'
;---
(define (repeated proc n)
    (if (= n 1)
        proc
        (lambda (x)
          (proc ((repeated proc (- n 1)) x)))))

;'rotate-frame-90'
;---
(define (rotate-frame-90 frame)
  (let ((t (transform-frame (make-vect 1.0 0.0)
                            (make-vect 1.0 1.0)
                            (make-vect 0.0 0.0))))
    (t frame)))

;'rotate-n'
;---
(define (rotate-n painter n)
  (lambda (frame)
    (let ((repeated-apply
           (repeated rotate-frame-90 n)))
      (painter (repeated-apply frame)))))

;'rotate-180'
;---
(define (rotate-180 painter)
  (rotate-n painter 2))

;'rotate-270'
;---
(define (rotate-270 painter)
  (rotate-n painter 3))


;------------------------------------------------------------------------------------------
;TESTS
;------------------------------------------------------------------------------------------

;test all implementations
;---
(display "flip-horiz") (newline)
(paint (flip-horiz einstein))
;---
(newline) (display "rotate180") (newline)
(paint (rotate180 einstein))
;---
(newline) (display "rotate270") (newline)
(paint (rotate270 einstein))
;---
(newline) (display "rotate-180") (newline)
(paint (rotate-180 einstein))
;---
(newline) (display "rotate-270") (newline)
(paint (rotate-270 einstein))

