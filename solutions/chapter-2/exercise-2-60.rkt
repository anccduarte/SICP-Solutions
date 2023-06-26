
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.60.
;---
;We specified that a set would be represented as a list with no duplicates. Now suppose
;we allow duplicates. For instance, the set {1, 2, 3} could be represented as the list
;(2 3 2 1 3 2 2). Design procedures 'element-of-set?', 'adjoin-set', 'union-set', and
;'intersection-set' that operate on this representation. How does the efficiency of each
;compare with the corresponding procedure for the non-duplicate representation? Are there
;applications for which you would use this representation in preference to the
;non-duplicate one?
;------------------------------------------------------------------------------------------

;'equal?' -> helper
;---
(define (equal? c1 c2)
  (or (eq? c1 c2)
      (and (pair? c1)
           (pair? c2)
           (equal? (car c1) (car c2))
           (equal? (cdr c1) (cdr c2)))))

;'element-of-set?'
;(the implementation does not suffer any alteration with the change of representation)
;---
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? (car set) x) true)
        (else (element-of-set? x (cdr set)))))

;'adjoin-set'
;(by alowing duplicates, when adjoining a new element to a given set, there is no need to
;check whether that element is already in the set)
;---
(define (adjoin-set x set)
  (cons x set))

;'intersection-set'
;(does not suffer any alteration with the change of representation)
;---
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2))
         '())
        ((element-of-set? (car set1) set2)
         (adjoin-set (car set1) (intersection-set (cdr set1) set2)))
        (else
         (intersection-set (cdr set1) set2))))

;'union-set'
;(there is no need to check whether an element of 'set1' is already an element of 'set2'
;in order to adjoin that element to 'set2'; note that the procedure as the exact same
;structure as 'append')
;---
(define (union-set set1 set2)
  (if (null? set1)
      set2
      (adjoin-set (car set1)
                  (union-set (cdr set1) set2))))


;test union and intersection for random sets
;---
(define (test set1 set2)
  (define (display-two x y)
    (display x) (display " ") (display y))
  (display "(intersection ") (display-two set1 set2) (display ") -> ")
  (display (intersection-set set1 set2))
  (newline)
  (display "(union ") (display-two set1 set2) (display ") -> ")
  (display (union-set set1 set2))
  (newline) (display "---") (newline))
;---
(test '() '())
(test '(1 2 3 4) '())
(test '() '(1 2 3 4))
(test '(1 2 3 4) '(3 4 5 6))
(test '(1 3 5 7) '(2 4 6 8))
(test '(1 2 3 3) '(1 1 3 4))

;conclusions on the new representation
;---
;every procedure that does not require 'element-of-set?' to be called, i.e., 'adjoin-set'
;and 'union-set', runs faster when compared to its original implementation. 'adjoin-set'
;is ought to simply 'cons' the new element to the preexisting set, transforming a O(n)
;process in an operation that executes in constant time. a similar reasoning is applied
;to 'union-set', as with this new representation every element 'x' of 'set1' must simply
;be added to 'set2' without checking whether 'x' already exists in 'set2' (a O(n^2)
;process is transformed in a O(n) operation)
;---
;regarding the remaining procedures, i.e., 'element-of-set?' and 'intersection-set', they
;will invariably run slower whenever the set representation contains at least a repeated
;element. since both procedures make use of 'element-of-set?' (and since this procedure
;has to potentially check all the elements of its argument set in order to return a
;value), the more elements are contained within a set (particularly repeated ones), the
;more time it is required for the computation to be performed

