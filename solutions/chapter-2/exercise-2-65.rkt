
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.65.
;---
;Use the results of Exercise 2.63 and Exercise 2.64 to give O(n) implementations of
;'union-set' and 'intersection-set' for sets implemented as (balanced) binary trees.
;------------------------------------------------------------------------------------------

;'make-tree' -> constructor for trees
;---
(define (make-tree entry left right)
  (list entry left right))

;selectors for trees
;---
(define entry car)
(define left-branch cadr)
(define right-branch caddr)

;'tree->list'
;(convert binary tree representation of sets to a list representation)
;---
(define (tree->list tree)
  (define (copy-to-list tree result)
    (if (null? tree)
        result
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result)))))
  (copy-to-list tree '()))

;'list->tree'
;(convert list representation of sets to a binary tree representation; alternative
;implementation to the one provided by the authors -> not sure if order of growth is O(n)
;as required)
;---
(define (add-item x lst)
  (if (null? lst)
      (cons x '())
      (cons (car lst)
            (add-item x (cdr lst)))))
;---
(define (split-list n lst)
  (define (iter-list n lst res)
    (if (= n 0)
        (list (car lst) res (cdr lst))
        (iter-list (- n 1)
                   (cdr lst)
                   (add-item (car lst) res))))
  (iter-list n lst '()))
;---
(define (list->tree lst)
  (cond ((null? lst) '())
        ((null? (cdr lst)) (make-tree (car lst) '() '()))
        (else
         (let ((mid (quotient (length (cdr lst)) 2)))
           (let ((parts (split-list mid lst)))
             (make-tree (car parts)
                        (list->tree (cadr parts))
                        (list->tree (caddr parts))))))))

;'union-set'
;(change the sets representation to lists [O(n)]; perform the union of the sets while in
;list format [O(n)]; change the representation of the result to a binary tree [probably
;O(n), but not sure]. since the three operations are independent of each other, the
;respective orders of growth may be added, resulting in an overall order of growth of
;O(n) [again, assuming that the order of growth of 'list->tree' is O(n)])
;---
(define (union-set set1 set2)
  (define (union-iter set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          (else
           (let ((x1 (car set1))
                 (x2 (car set2)))
             (cond ((= x1 x2)
                    (cons x1 (union-iter (cdr set1) (cdr set2))))
                   ((< x1 x2)
                    (cons x1 (union-iter (cdr set1) set2)))
                   ((> x1 x2)
                    (cons x2 (union-iter set1 (cdr set2)))))))))
  (let ((union-list
         (union-iter (tree->list set1)
                     (tree->list set2))))
    (list->tree union-list)))

;'intersection-set'
;(same idea as in 'union-set')
;---
(define (intersection-set set1 set2)
  (define (intersection-iter set1 set2)
    (if (or (null? set1) (null? set2))
        '()
        (let ((x1 (car set1))
              (x2 (car set2)))
          (cond ((= x1 x2)
                 (cons x1 (intersection-iter (cdr set1) (cdr set2))))
                ((< x1 x2)
                 (intersection-iter (cdr set1) set2))
                ((> x1 x2)
                 (intersection-iter set1 (cdr set2)))))))
  (let ((intersection-list
         (intersection-iter (tree->list set1)
                            (tree->list set2))))
    (list->tree intersection-list)))

;test for random sets
;---
(define set1 '(7
               (3 (1 () ()) (5 () ()))
               (9 () (11 () ()))))
;---
(define set2 '(3
               (1 () ())
               (7 (5 () ()) (9 () (13 () ())))))
;---
(display "set1 -> ") set1
(display "set2 -> ") set2
(display "union -> ") (union-set set1 set2)
(display "intersection -> ") (intersection-set set1 set2)

