
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.26.
;---
;Suppose we define 'x' and 'y' to be two lists:
;---
;(define x (list 1 2 3))
;(define y (list 4 5 6))
;---
;What result is printed by the interpreter in response to evaluating each of the
;following expressions:
;---
;1. (append x y)
;2. (cons x y)
;3. (list x y)
;------------------------------------------------------------------------------------------

;defining lists 'x' and 'y'
;---
(define x (list 1 2 3))
(define y (list 4 5 6))

;defining 'append'
;---
(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1)
            (append (cdr l1) l2))))

;1.
;---
(append x y)
;'append' merges the lists it takes as input into a single flat list
;hence, (append x y) -> (1 2 3 4 5 6)

;2.
;---
(cons x y)
;'cons' merges the first list and the elements of the second list
;---
;(cons x y)
;(cons (list 1 2 3) (list 4 5 6))
;(cons (list 1 2 3) (cons 4 (cons 5 (cons 6 nil))))
;            |            |       |       |
;         (1 2 3)         4       5       6
;---
;hence, (cons x y) -> ((1 2 3) 4 5 6)

;3.
;---
(list x y)
;'list' merges the two lists into a list of lists 'x' and 'y'
;---
;(list x y)
;(cons x (cons y nil))
;(cons (list 1 2 3) (cons (list 4 5 6) nil))
;            |                  |
;         (1 2 3)            (4 5 6)
;---
;hence, (list x y) -> ((1 2 3) (4 5 6))

