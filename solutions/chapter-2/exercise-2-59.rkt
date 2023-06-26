
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.59.
;---
;Implement the 'union-set' operation for the unordered-list representation of sets.
;------------------------------------------------------------------------------------------

;'equal?' -> helper
;---
(define (equal? c1 c2)
  (or (eq? c1 c2)
      (and (pair? c1)
           (pair? c2)
           (equal? (car c1) (car c2))
           (equal? (cdr c1) (cdr c2)))))

;'element-of-set?' -> helper
;---
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? (car set) x) true)
        (else (element-of-set? x (cdr set)))))

;'union-set'
;---
(define (union-set set1 set2)
  (cond ((null? set1)
         set2)
        ((not (element-of-set? (car set1) set2))
         (cons (car set1) (union-set (cdr set1) set2)))
        (else
         (union-set (cdr set1) set2))))

;test for random sets
;---
(define (test set1 set2)
  (display "(union-set ") (display set1) (display " ") (display set2) (display ") -> ")
  (display (union-set set1 set2))
  (newline))
;---
(test '() '())
(test '(1 2 3 4) '())
(test '() '(1 2 3 4))
(test '(1 2 3 4) '(3 4 5 6))
(test '(1 3 5 7) '(2 4 6 8))

