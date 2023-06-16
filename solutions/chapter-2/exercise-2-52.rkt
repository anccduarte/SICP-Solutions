
#lang sicp
(#%require sicp-pict)

;------------------------------------------------------------------------------------------
;EXERCISE 2.52.
;---
;Make changes to the square limit of 'wave' shown in Figure 2.9 by working at each of the
;levels described above. In particular:
;---
;a. Add some segments to the primitive 'wave' painter of Exercise 2.49 (to add a smile,
;for example).
;---
;b. Change the pattern constructed by 'corner-split' (for example, by using only one copy
;of the 'up-split' and 'right-split' images instead of two).
;---
;c. Modify the version of 'square-limit' that uses 'square-of-four' so as to assemble the
;corners in a different pattern. (For example, you might make the big Mr. Rogers look
;outward from each corner of the square.)
;------------------------------------------------------------------------------------------

;the following procedures (available in 'sicp-pict') are used for convenience:
;- 'make-vect'
;- 'make-segment'
;- 'segments->painter'
;- 'flip-horiz', 'flip-vert', 'beside', 'below'

;a. defining a new version of 'wave'
;---
;original 'wave'
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
;add a smile
(define seg-d15 (make-segment (make-vect 0.4 0.8) (make-vect 0.5 0.7)))
(define seg-d16 (make-segment (make-vect 0.5 0.7) (make-vect 0.6 0.8)))
;---
(define wave-list (list seg-d1 seg-d2 seg-d3 seg-d4
                        seg-d5 seg-d6 seg-d7 seg-d8
                        seg-d9 seg-d10 seg-d11 seg-d12
                        seg-d13 seg-d14 seg-d15 seg-d16))
;---
(define wave (segments->painter wave-list))

;b. defining a new rule for 'corner-split' 
;---
(define (split painter n op1 op2)
  (if (= n 0)
      painter
      (let ((smaller (split painter (- n 1) op1 op2)))
        (op1 painter (op2 smaller smaller)))))
;---
(define (right-split painter n)
  (split painter n beside below))
;---
(define (up-split painter n)
  (split painter n below beside))
;---
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1)))
            (corner (corner-split painter (- n 1))))
        (beside (below painter up)
                (below right corner)))))

;c. defining a new rule for 'square-limit'
;---
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))
;---
(define (square-limit painter n)
  (let ((corner (corner-split painter n)))
    ((square-of-four identity
                     flip-horiz
                     flip-vert
                     rotate180)
     corner)))

;final result
;---
(define (for-each proc lst)
  (cond ((null? lst)
         (display ""))
        (else
         (proc (car lst))
         (for-each proc (cdr lst)))))
;---
(for-each (lambda (n)
            (display (paint (square-limit wave n)))
            (display "  "))
          (list 0 1 2 3 4))

