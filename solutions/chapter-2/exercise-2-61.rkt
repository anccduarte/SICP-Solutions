
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.61.
;---
;Give an implementation of 'adjoin-set' using the ordered representation. By analogy with
;'element-of-set?' show how to take advantage of the ordering to produce a procedure that
;requires on the average about half as many steps as with the unordered representation.
;------------------------------------------------------------------------------------------

;'element-of-set?'
;(here, we take advantage of the fact that the set is ordered to perform an additional
;test case: if the first element of the set is larger than the element 'e' we are looking
;for, then, we can conclude that 'e' is not contained in the set since all the remaining
;elements are also larger than 'e'. in the worst case scenario, i.e., 'e' is larger than
;or equal to the last element of the set, the lookup speed associated to this
;representation remains unaltered. however, we can expect a 2x speedup in the average
;case - although the time complexity is still O(n))
;---
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= (car set) x) true)
        ((> (car set) x) false)
        (else (element-of-set? x (cdr set)))))

;'adjoin-set'
;(the proposed representation does not allow to simply 'cons' the new element 'e' onto
;the set 's' if 'e' is not present in 's'; 'e' must be adequately introduced in 's' in
;order to preserve the ordering of the set)
;---
(define (adjoin-set x set)
  (cond ((null? set) (cons x '()))
        ((= (car set) x) set)
        ((> (car set) x) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

;test
;---
(define (test x set)
  (define (display-two x y)
    (display x) (display " ") (display y))
  (display "(adjoin-set ") (display-two x set) (display ") -> ")
  (display (adjoin-set x set))
  (newline))
;---
(for-each (lambda (x) (test x '(1 2 3 4)))
          '(0 1 2 3 4 5 6))

