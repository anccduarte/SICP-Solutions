
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.38.
;---
;The 'accumulate' procedure is also known as 'fold-right', because it combines the first
;element of the sequence with the result of combining all the elements to the right.
;There is also a 'fold-left', which is similar to 'fold-right', except that it combines
;elements working in the opposite direction (SEE BELOW *). What are the values of:
;---
;(fold-right / 1 (list 1 2 3))
;(fold-left / 1 (list 1 2 3))
;(fold-right list nil (list 1 2 3))
;(fold-left list nil (list 1 2 3))
;---
;Give a property that 'op' should satisfy to guarantee that 'fold-right' and 'fold-left'
;will produce the same values for any sequence.
;------------------------------------------------------------------------------------------

;'fold-right' / 'accumulate'
;(the first element to be combined with 'init' is the last element of 'lst')
;---
(define (fold-right op init lst)
  (if (null? lst)
      init
      (op (car lst)
          (fold-right op init (cdr lst)))))

;'fold-left'
;(the first element to be combined with 'init' is the first element of 'lst')
;---
(define (fold-left op init lst)
  (define (iter res rest)
    (if (null? rest)
        res
        (iter (op res (car rest))
              (cdr rest))))
  (iter init lst))

;evaluating (fold-right / 1 (list 1 2 3))
;---
(fold-right / 1 (list 1 2 3))
;(/ 1 (fold-right / 1 (list 2 3)))
;(/ 1 (/ 2 (fold-right / 1 (list 3))))
;(/ 1 (/ 2 (/ 3 (fold-right / 1 nil))))
;(/ 1 (/ 2 (/ 3 1)))
;3/2

;evaluating (fold-left / 1 (list 1 2 3))
;---
(fold-left / 1 (list 1 2 3))
;(iter 1 (list 1 2 3))
;(iter (/ 1 1) (list 2 3))
;(iter (/ (/ 1 1) 2) (list 3))
;(iter (/ (/ (/ 1 1) 2) 3) nil)
;(/ (/ (/ 1 1) 2) 3)
;1/6

;evaluating (fold-right list nil (list 1 2 3))
;---
(fold-right list nil (list 1 2 3))
;(list 1 (fold-right list nil (list 2 3)))
;(list 1 (list 2 (fold-right list nil (list 3))))
;(list 1 (list 2 (list 3 (fold-right list nil nil))))
;(list 1 (list 2 (list 3 nil)))
;(1 (2 (3 ())))

;evaluating (fold-left list nil (list 1 2 3))
;---
(fold-left list nil (list 1 2 3))
;(iter nil (list 1 2 3))
;(iter (list nil 1) (list 2 3))
;(iter (list (list nil 1) 2) (list 3))
;(iter (list (list (list nil 1) 2) 3) nil)
;(list (list (list nil 1) 2) 3)
;(((() 1) 2) 3)

;answer to last part of the question
;---
;in order that 'fold-right' and 'fold-left' produce the same result, the operation 'op'
;must obey the commutative property, that is, for any two inputs 'x' and 'y', the result
;of (op x y) must be the same as (op y x). such operations include, for example, addition
;and multiplication
;---
(fold-right + 0 (list 1 2 3 4)) ;10
(fold-left + 0 (list 1 2 3 4)) ;10
;---
(fold-right * 1 (list 1 2 3 4)) ;24
(fold-left * 1 (list 1 2 3 4)) ;24

