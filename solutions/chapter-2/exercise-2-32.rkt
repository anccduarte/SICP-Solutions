
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.32.
;---
;We can represent a set as a list of distinct elements, and we can represent the set of
;all subsets of the set as a list of lists. For example, if the set is (1 2 3), then the
;set of all subsets is (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)). Complete the following
;definition of a procedure that generates the set of subsets of a set and give a clear
;explanation of why it works:
;---
;(define (subsets s)
;  (if (null? s)
;      (list nil)
;      (let ((rest (subsets (cdr s))))
;        (append rest (map <??> rest)))))
;------------------------------------------------------------------------------------------

;'subsets'
;---
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (sub)
                            (cons (car s) sub))
                          rest)))))

;test for the set (1 2 3)
;---
(define lst (list 1 2 3))
(subsets lst)

;explanation
;---
;the procedure works as follows:
;- if the input of 'subsets' is nil, return a list containing the empty list (nil)
;- otherwise, let 'rest' be the result of calling 'subsets' on the 'cdr' of the list, and
;  return 'rest' concatenated to a list consisting of the subsets contained within 'rest'
;  with the 'car' of the original list added to them
;---
;that is:
;subsets(lst) = list(nil), if null?(lst)
;               subsets(cdr(lst)) + map(add-car-of-lst, subsets(cdr(lst))), otherwise

