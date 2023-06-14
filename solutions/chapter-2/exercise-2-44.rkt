
#lang sicp
(#%require sicp-pict)

;------------------------------------------------------------------------------------------
;EXERCISE 2.44.
;---
;Define the procedure 'up-split' used by 'corner-split'. It is similar to 'right-split',
;except that it switches the roles of 'below' and 'beside'.
;------------------------------------------------------------------------------------------

;'up-split'
;---
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;test for 'einstein' painter
;---
(define (for-each proc lst)
  (cond ((null? lst)
         (display ""))
        (else
         (proc (car lst))
         (for-each proc (cdr lst)))))
;---
(for-each (lambda (x)
            (display (paint (up-split einstein x)))
            (display " "))
          (list 1 2 3))

