
#lang sicp
(#%require sicp-pict)

;------------------------------------------------------------------------------------------
;1st LEVEL OF ABSTRACTION
;(construction of painters from a list of segments, for example)
;------------------------------------------------------------------------------------------

;map the coordinates of a vector into a frame
;---
(define (frame-coord-map frame)
  (lambda (vec)
    (vector-add (frame-origin frame)
                (vector-add (vector-scale (vector-xcor vec)
                                          (frame-edge1 frame))
                            (vector-scale (vector-ycor vec)
                                          (frame-edge2 frame))))))

;placeholer for drawing a line between two points
;---
(define (draw-line start end)
  (display start) (display " ---> ") (display end))

;draw segments on screen from a list of segments
;---
(define (segments-to-painter seglist)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (for-each (lambda (s)
                  (draw-line (m (segment-start s))
                             (m (segment-end s))))
                seglist))))

;------------------------------------------------------------------------------------------
;2nd LEVEL OF ABSTRACTION
;(geometric transformations and combinations of painters)
;------------------------------------------------------------------------------------------

;frame tranformation according to new coordinate references -> helper
;---
(define (transform-frame origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (make-frame new-origin
                    (vector-sub (m corner1) new-origin)
                    (vector-sub (m corner2) new-origin))))))

;identity
;---
(define (identity painter)
  (lambda (frame)
    (let ((t (transform-frame (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              (make-vect 0.0 1.0))))
      (painter (t frame)))))

;horizontal flip
;---
(define (flip-horiz painter)
  (lambda (frame)
    (let ((t
           (transform-frame (make-vect 1.0 0.0)
                            (make-vect 0.0 0.0)
                            (make-vect 1.0 1.0))))
      (painter (t frame)))))

;vertical flip
;---
(define (flip-vert painter)
  (lambda (frame)
    (let ((t
           (transform-frame (make-vect 0.0 1.0)
                            (make-vect 1.0 1.0)
                            (make-vect 0.0 0.0))))
      (painter (t frame)))))

;rotate frame by 90 degrees -> helper
;---
(define (rotate-frame-90 frame)
  (let ((t (transform-frame (make-vect 1.0 0.0)
                            (make-vect 1.0 1.0)
                            (make-vect 0.0 0.0))))
    (t frame)))

;repeated application of a procedure -> helper
;---
(define (repeat proc n)
  (if (= n 1)
      proc
      (lambda (x)
        (proc ((repeat proc (- n 1))
               x)))))

;rotate painter by n*90 degrees -> helper
;---
(define (rotate-n painter n)
  (lambda (frame)
    (let ((r (repeat rotate-frame-90 n)))
      (painter (r frame)))))

;rotate painter by 90 degrees (trivial having defined "rotate-n")
;---
(define (rotate-90 painter)
  (rotate-n painter 1))

;rotate painter by 180 degrees (trivial having defined "rotate-n")
;---
(define (rotate-180 painter)
  (rotate-n painter 2))

;rotate painter by 270 degrees (trivial having defined "rotate-n")
;---
(define (rotate-270 painter)
  (rotate-n painter 3))

;place one painter beside another painter
;---
(define (my-beside painter1 painter2 s)
  (lambda (frame)
    (let ((lt (transform-frame (make-vect 0.0 0.0)
                               (make-vect s 0.0)
                               (make-vect 0.0 1.0)))
          (rt (transform-frame (make-vect s 0.0)
                               (make-vect 1.0 0.0)
                               (make-vect s 1.0))))
      (painter1 (lt frame))
      (painter2 (rt frame)))))

;place one painter below another painter
;---
(define (my-below painter1 painter2 s)
  (rotate-90 (my-beside (rotate-270 painter1)
                        (rotate-270 painter2)
                        s)))

;------------------------------------------------------------------------------------------
;3rd LEVEL OF ABSTRACTION
;(assemblage of geometric transformations and combinations of painters)
;------------------------------------------------------------------------------------------

;push painter towards an arbitrary direction -> helper
;---
(define (push painter operation n)
  (if (= n 0)
      painter
      (operation painter
                 (push painter operation (- n 1))
                 0.5)))

;push painter towards the right edge of a frame
;---
(define (push-right painter n)
  (push painter my-beside n))

;push painter towards the top edge of a frame
;---
(define (push-top painter n)
  (push painter my-below n))

;split painter towards arbitrary direction -> helper
;---
(define (split painter op1 op2 n)
  (if (= n 0)
      painter
      (let ((smaller (split painter op1 op2 (- n 1))))
        (op1 painter
             (op2 smaller smaller 0.5)
             0.5))))

;split painter towards the right edge of the frame
;---
(define (right-split painter n)
  (split painter my-beside my-below n))

;split painter towards the top edge of the frame
;---
(define (up-split painter n)
  (split painter my-below my-beside n))

;split painter towards the right and the top edges of a frame
;---
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((right (right-split painter (- n 1)))
            (top (up-split painter (- n 1))))
        (let ((bottom-right (my-below right right 0.5))
              (top-left (my-beside top top 0.5))
              (corner (corner-split painter (- n 1))))
          (my-beside (my-below painter top-left 0.5)
                     (my-below bottom-right corner 0.5)
                     0.5)))))

;combine 4 instances of a painter in a single frame
;---
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (my-below (my-beside (bl painter)
                         (br painter)
                         0.5)
              (my-beside (tl painter)
                         (tr painter)
                         0.5)
              0.5)))

;square limit of a painter (arrangement of 4 instances of a painter)
;---
(define (square-limit painter n)
  (let ((corner (corner-split painter n))
        (comb4 (square-of-four flip-horiz
                               identity
                               rotate-180
                               flip-vert)))
    (comb4 corner)))

;------------------------------------------------------------------------------------------
;TESTS
;------------------------------------------------------------------------------------------

(display "ORIGINAL EINSTEIN PAINTER") (newline)
(display (paint (identity einstein)))

(newline) (display "TRANSFORMATIONS ON A SINGLE PAINTER") (newline)
(display (paint (flip-horiz einstein)))
(display "  ")
(display (paint (flip-vert einstein)))
(display "  ")
(display (paint (rotate-90 einstein)))
(display "  ")
(display (paint (rotate-180 einstein)))
(display "  ")
(display (paint (rotate-270 einstein)))

(newline) (display "COMBINATIONS OF PAINTERS") (newline)
(display (paint (my-beside einstein einstein 0.5)))
(display "  ")
(display (paint (my-beside einstein einstein 0.75)))
(display "  ")
(display (paint (my-below einstein einstein 0.5)))
(display "  ")
(display (paint (my-below einstein einstein 0.75)))

(newline) (display "ASSEMBLY OF GEOMETRIC OPERATIONS") (newline)
(display (paint (push-right einstein 3)))
(display "  ")
(display (paint (push-top einstein 3)))
(display "  ")
(display (paint (right-split einstein 3)))
(display "  ")
(display (paint (up-split einstein 3)))
(display "  ")
(display (paint (corner-split einstein 3)))
(display "  ")
(display (paint (square-limit einstein 3)))

