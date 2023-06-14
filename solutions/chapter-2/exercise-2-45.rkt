
#lang sicp
(#%require sicp-pict)

;------------------------------------------------------------------------------------------
;EXERCISE 2.45.
;---
;'right-split' and 'up-split' can be expressed as instances of a general splitting
;operation. Define a procedure 'split' with the property that evaluating
;---
;(define right-split (split beside below))
;(define up-split (split below beside))
;---
;produces procedures 'right-split' and 'up-split' with the same behaviors as the ones
;already defined.
;------------------------------------------------------------------------------------------

;'split'
;(general procedure abstracting the operation of splitting a painter)
;---
(define (split op1 op2)
  (define (splitter painter n)
    (if (= n 0)
        painter
        (let ((smaller (splitter painter (- n 1))))
          (op1 painter (op2 smaller smaller)))))
  splitter)

;defining 'up-split' and 'right-split' in terms of 'split'
;---
(define up-split (split below beside))
(define right-split (split beside below))

;test for 'einstein' painter
;---
(define (for-each proc lst)
  (cond ((null? lst)
         (display ""))
      (else
       (proc (car lst))
       (for-each proc (cdr lst)))))
;---
(display "RIGHT-SPLIT") (newline)
(for-each (lambda (x)
            (display (paint (right-split einstein x)))
            (display " "))
          (list 1 2 3))
;---
(newline) (newline) (display "UP-SPLIT") (newline)
(for-each (lambda (x)
            (display (paint (up-split einstein x)))
            (display " "))
          (list 1 2 3))

