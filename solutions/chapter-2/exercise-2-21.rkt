
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.21.
;---
;The procedure 'square-list' takes a list of numbers as argument and returns a list of
;the squares of those numbers. Here are two different definitions of 'square-list'.
;Complete both of them by filling in the missing expressions:
;---
;(define (square-list items)
;  (if (null? items)
;      nil
;      (cons <??> <??>)))
;---
;(define (square-list items)
;  (map <??> <??>))
;------------------------------------------------------------------------------------------

;"The difference between the two definitions is not that the computer is performing a
;different process (it isn't) but that we think about the process differently. In effect,
;'map' helps establish an abstraction barrier that isolates the implementation of
;procedures that transform lists from the details of how the elements of the list are
;extracted and combined."

;'square'
;---
(define square (lambda (x) (* x x)))

;version 1 -> body of 'square-list' not abstracted by 'map'
;---
(define (square-list-1 items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list-1 (cdr items)))))

;'map'
;---
(define (map proc lst)
  (if (null? lst)
      nil
      (cons (proc (car lst))
            (map proc (cdr lst)))))

;version 2 -> 'square-list' in terms of 'map'
;---
(define (square-list-2 items)
  (map square items))

;test both versions
;---
(square-list-1 (list 1 2 3 4))
(square-list-2 (list 1 2 3 4))

