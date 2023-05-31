
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.17.
;---
;Define a procedure 'last-pair' that returns the list that contains only the last element
;of a given (nonempty) list:
;---
;(last-pair (list 23 72 149 34)) -> 34
;------------------------------------------------------------------------------------------

;'last-pair'
;---
(define (last-pair lst)
  (if (null? (cdr lst))
      lst
      (last-pair (cdr lst))))

;test
;---
(last-pair (list 23 72 149 34))

