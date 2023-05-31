
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.30.
;---
;Define a procedure 'square-tree' analogous to the 'square-list' procedure of
;Exercise 2.21. That is, 'square-tree' should behave as follows:
;---
;(square-tree
; (list 1
;       (list 2
;             (list 3 4)
;             5)
;       (list 6 7))) -> (1 (4 (9 16) 25) (36 49))
;---
;Define 'square-tree' both directly (i.e., without using any higher-order procedures) and
;also by using 'map' and recursion.
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
            (map proc (cdr lst)))))

;'square-tree-1' -> without higher-order procedures
;---
(define (square-tree-1 tree)
  (if (null? tree)
      nil
      (let ((head (car tree)))
        (cons (if (not (pair? head))
                  (square head)
                  (square-tree-1 head))
              (square-tree-1 (cdr tree))))))

;'square-tree-2' -> in terms of 'map'
;---
(define (square-tree-2 tree)
  (my-map (lambda (sub-tree)
            (if (not (pair? sub-tree))
                (square sub-tree)
                (square-tree-2 sub-tree)))
          tree))

;test both versions
;---
(define tree (list 1
                   (list 2
                         (list 3 4)
                         5)
                   (list 6 7)))
;---
(square-tree-1 tree)
(square-tree-2 tree)

