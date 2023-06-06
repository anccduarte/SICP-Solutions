
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.33.
;---
;Fill in the missing expressions to complete the following definitions of some basic
;list-manipulation operations as accumulations:
;---
;(define (map p sequence)
;  (accumulate (lambda (x y) <??>) nil sequence))
;---
;(define (append seq1 seq2)
;  (accumulate cons <??> <??>))
;---
;(define (length sequence)
;  (accumulate <??> 0 sequence))
;------------------------------------------------------------------------------------------

;'accumulate'
;---
(define (accumulate op initial lst)
  (if (null? lst)
      initial
      (op (car lst)
          (accumulate op initial (cdr lst)))))

;'map' in terms of 'accumulate'
;---
(define (map proc lst)
  (accumulate (lambda (x y) (cons (proc x) y))
              nil
              lst))

;'append' in terms of 'accumulate'
;(remember the previous definition of 'append' -> if lst1 is nil, return lst2)
;---
(define (append lst1 lst2)
  (accumulate cons lst2 lst1))

;'length' in terms of 'accumulate'
;---
(define (length lst)
  (accumulate (lambda (x y) (inc y))
              0
              lst))

;test
;---
(define lst (list 1 2 3 4))
(display "lst -> (list 1 2 3 4)") (newline)
(display "(map square lst) -> ") (map (lambda (x) (* x x)) lst)
(display "(append lst lst) -> ") (append lst lst)
(display "(length lst) -> ") (length lst)

