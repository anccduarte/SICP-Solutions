
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.28.
;---
;Write a procedure 'fringe' that takes as argument a tree (represented as a list) and
;returns a list whose elements are all the leaves of the tree arranged in left-to-right
;order. For example,
;---
;(define x (list (list 1 2) (list 3 4)))
;(fringe x) -> (1 2 3 4)
;(fringe (list x x)) -> (1 2 3 4 1 2 3 4)
;------------------------------------------------------------------------------------------

;'append' -> helper procedure
;---
(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1)
            (append (cdr l1) l2))))

;'fringe'
;---
(define (fringe tree)
  (if (null? tree)
      nil
      (let ((head (car tree)))
        (append (if (not (pair? head))
                    (list head)
                    (fringe head))
                (fringe (cdr tree))))))

;test
;---
(define x (list (list 1 2) (list 3 4)))
(fringe x)
(fringe (list x x))

