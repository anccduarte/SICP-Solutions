
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.31.
;---
;Abstract your answer to Exercise 2.30 to produce a procedure 'tree-map' with the
;property that 'square-tree' could be defined as:
;---
;(define (square-tree tree) (tree-map square tree))
;------------------------------------------------------------------------------------------

;'square' -> helper procedure
;---
(define square (lambda (x) (* x x)))

;'my-map' -> helper procedure
;---
(define (my-map proc lst)
  (if (null? lst)
      nil
      (cons (proc (car lst))
            (my-map proc (cdr lst)))))

;'tree-map'
;---
(define (tree-map proc tree)
  (my-map (lambda (sub-tree)
            (if (not (pair? sub-tree))
                (proc sub-tree)
                (tree-map proc sub-tree)))
          tree))

;'square-tree'
;---
(define (square-tree tree)
  (tree-map square tree))

;test for tree of exercise 2.30.
;---
(define tree (list 1
                   (list 2
                         (list 3 4)
                         5)
                   (list 6 7)))
;---
(square-tree tree)

