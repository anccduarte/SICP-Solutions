
#lang sicp
(#%require sicp-pict)

;------------------------------------------------------------------------------------------
;EXERCISE 2.49.
;---
;Use 'segments->painter' to define the following primitive painters:
;---
;a. The painter that draws the outline of the designated frame.
;b. The painter that draws an 'X' by connecting opposite corners of the frame.
;c. The painter that draws a diamond shape by connecting the midpoints of the sides of
;   the frame.
;d. The 'wave' painter.
;------------------------------------------------------------------------------------------

;the following procedures (available in 'sicp-pict') are used for convenience:
;---
;- 'make-vect', 'vector-xcor', 'vector-ycor'
;- 'vector-add', 'vector-scale'
;- 'make-frame', 'frame-origin', 'frame-edge1', 'frame-edge2'
;- 'make-segment', 'segment-start', 'segment-end'
;- 'segments->painter' (textbook implementation below)

;'frame-coord-map' -> helper (textbook implementation)
;---
(define (frame-coord-map frame)
  (lambda (vec)
    (vector-add (frame-origin frame)
                (vector-add (vector-scale (vector-xcor vec)
                                          (frame-edge1 frame))
                            (vector-scale (vector-ycor vec)
                                          (frame-edge2 frame))))))

;'segments->painter' -> helper (textbook implementation)
;---
(define (for-each proc lst)
  (cond ((null? lst)
         (display ""))
        (else
         (proc (car lst))
         (for-each proc (cdr lst)))))
;---
(define (draw-line start end)
  (display "Drawn line from ")
  (display start)
  (display " to ")
  (display end)
  (newline))
;---
(define (my-segments->painter seg-list)
  (lambda (frame)
    (let ((coord-map (frame-coord-map frame)))
      (for-each (lambda (segment)
                  (draw-line
                   (coord-map (segment-start segment))
                   (coord-map (segment-end segment))))
                seg-list))))

;a.
;---
(define seg-a1 (make-segment (make-vect 0 0) (make-vect 0 1)))
(define seg-a2 (make-segment (make-vect 0 1) (make-vect 1 1)))
(define seg-a3 (make-segment (make-vect 1 1) (make-vect 1 0)))
(define seg-a4 (make-segment (make-vect 1 0) (make-vect 0 0)))
;---
(define seg-list-a (list seg-a1 seg-a2 seg-a3 seg-a4))
;---
(define painter-a (segments->painter seg-list-a))
(display (paint painter-a))

;b.
;---
(define seg-b1 (make-segment (make-vect 0 0) (make-vect 1 1)))
(define seg-b2 (make-segment (make-vect 0 1) (make-vect 1 0)))
;---
(define seg-list-b (list seg-b1 seg-b2))
;---
(define painter-b (segments->painter seg-list-b))
(display "  ") (display (paint painter-b))

;c.
;---
(define seg-c1 (make-segment (make-vect 0 0.5) (make-vect 0.5 1)))
(define seg-c2 (make-segment (make-vect 0.5 1) (make-vect 1 0.5)))
(define seg-c3 (make-segment (make-vect 1 0.5) (make-vect 0.5 0)))
(define seg-c4 (make-segment (make-vect 0.5 0) (make-vect 0 0.5)))
;---
(define seg-list-c (list seg-c1 seg-c2 seg-c3 seg-c4))
;---
(define painter-c (segments->painter seg-list-c))
(display "  ") (display (paint painter-c))

;d.
;---
(define seg-d1 (make-segment (make-vect 0 0.6) (make-vect 0.2 0.4)))
(define seg-d2 (make-segment (make-vect 0 0.7) (make-vect 0.2 0.5)))
(define seg-d3 (make-segment (make-vect 0.2 0.4) (make-vect 0.4 0.5)))
(define seg-d4 (make-segment (make-vect 0.2 0.5) (make-vect 0.4 0.6)))
(define seg-d5 (make-segment (make-vect 0.2 0) (make-vect 0.4 0.5)))
(define seg-d6 (make-segment (make-vect 0.3 0) (make-vect 0.5 0.4)))
(define seg-d7 (make-segment (make-vect 0.5 0.4) (make-vect 0.7 0)))
(define seg-d8 (make-segment (make-vect 0.6 0.5) (make-vect 0.8 0)))
(define seg-d9 (make-segment (make-vect 0.6 0.5) (make-vect 1 0.3)))
(define seg-d10 (make-segment (make-vect 0.6 0.6) (make-vect 1 0.4)))
(define seg-d11 (make-segment (make-vect 0.3 0.8) (make-vect 0.4 0.6)))
(define seg-d12 (make-segment (make-vect 0.3 0.8) (make-vect 0.4 1)))
(define seg-d13 (make-segment (make-vect 0.6 0.6) (make-vect 0.7 0.8)))
(define seg-d14 (make-segment (make-vect 0.6 1) (make-vect 0.7 0.8)))
;---
(define seg-list-d (list seg-d1 seg-d2 seg-d3 seg-d4 seg-d5 seg-d6 seg-d7
                         seg-d8 seg-d9 seg-d10 seg-d11 seg-d12 seg-d13 seg-d14))
;---
(define waver (segments->painter seg-list-d))
(display "  ") (display (paint waver))
;---
(define waver2 (beside waver (flip-vert waver)))
(define waver4 (below (flip-vert waver2) waver2))
(display "  ") (display (paint waver4))

;extra -> square limit of 'waver4'
;('corner-split' implementation is slightly different from the one presented in the
;textbook; as a result 'square-limit' produces a distinct output)
;---
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((top (up-split painter (- n 1))))
        (below painter (beside top top)))))
;---
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((right (right-split painter (- n 1))))
        (beside painter (below right right)))))
;---
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((top (up-split painter (- n 1)))
            (right (right-split painter (- n 1)))
            (corner (corner-split painter (- n 1))))
        (beside (below painter top)
                (below right corner)))))
;---
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))
;---
(newline) (newline)
(for-each (lambda (x)
            (display (paint (square-limit waver4 x)))
            (display "  "))
          (list 0 1 2 3 4))

