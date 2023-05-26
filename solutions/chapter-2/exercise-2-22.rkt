
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.22.
;---
;Louis Reasoner tries to rewrite the first 'square-list' procedure of Exercise 2.21 so
;that it evolves an iterative process (SEE BELOW *). Unfortunately, defining
;'square-list' this way produces the answer list in the reverse order of the one desired.
;Why? Louis then tries to fix his bug by interchanging the arguments to 'cons' (SEE
;BELOW **). This doesn't work either. Explain.
;------------------------------------------------------------------------------------------

;'square'
;---
(define square (lambda (x) (* x x)))

;(*) 1st implementation
;---
(define (square-list-1 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

;example and explanation for 1st implementation
;---
(display "'square-list-1' -> ") (square-list-1 (list 1 2 3 4))
;---
;initially, the answer is the end of list marker (nil). at every iteration, we 'cons' the
;square of the 'car' of 'items' onto the answer attained at the previous iteration. this
;means that, although we square all the elements in 'items', they are glued together by
;'cons' in reverse order. for example, applying 'square-list-1' to (list 1 2 3 4) results
;in the following:
;---
;(square-list-1 (list 1 2 3 4))
;(iter (list 1 2 3 4) nil)
;(iter (list 2 3 4) (cons 1 nil))
;(iter (list 3 4) (cons 4 (cons 1 nil)))
;(iter (list 4) (cons 9 (cons 4 (cons 1 nil))))
;(iter nil (cons 16 (cons 9 (cons 4 (cons 1 nil)))))
;(cons 16 (cons 9 (cons 4 (cons 1 nil))))
;(16 9 4 1)

;(**) 2nd implementation
;---
(define (square-list-2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

;example and explanation for 2nd implementation
;---
(display "'square-list-2' -> ") (square-list-2 (list 1 2 3 4))
;---
;this second implementation of 'square-list' actually squares all the numbers in the list
;it takes as input and returns all the squared elements in the appropriate order.
;however, the process of gluing elements together via 'cons' is performed in reverse,
;that is, the 'car' of each pair is itself a pair (or nil), and the 'cdr' is a number.
;this results in an output with of the form ( ... (((() . n1^2) . n2^2) . n3^2) ... ).
;for example, apllying this new version of 'square-list' to the list (list 1 2 3 4)
;results in the following:
;---
;(square-list-2 (list 1 2 3 4))
;(iter (list 1 2 3 4) nil)
;(iter (list 2 3 4) (cons nil 1))
;(iter (list 3 4) (cons (cons nil 1) 4))
;(iter (list 4) (cons (cons (cons nil 1) 4) 9))
;(iter nil (cons (cons (cons (cons nil 1) 4) 9) 16))
;(cons (cons (cons (cons nil 1) 4) 9) 16)
;((((() . 1) . 4) . 9) . 16)

