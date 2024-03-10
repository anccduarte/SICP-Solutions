
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.18.
;---
;Write a procedure that examines a list and determines whether it contains a cycle, that
;is, whether a program that tried to find the end of the list by taking successive 'cdrs'
;would go into an infinite loop. Exercise 3.13 constructed such lists.
;------------------------------------------------------------------------------------------

;'contains-cycle?'
;(it basically implements a specialized version of depth-first search)
;---
(define (contains-cycle? x)
  ;---
  (define (iter p visited)
    (cond ((not (pair? p)) #f) ;'() isn't a pair
          ((memq p visited) #t)
          (else
           (or (iter (car p) (cons p visited))
               (iter (cdr p) (cons p visited))))))
  ;---
  (iter x '()))

;auxiliary procedures
;---
(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))
;---
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

;'test'
;---
(define (test x)
  (display x)
  (display " -> ")
  (display (contains-cycle? x))
  (newline))

;clearly there are no cycles
;---
(let ((x '(a b c d)))
  (test x)) ;#f
;---
(let ((x (cons 'a (cons 'b (cons 'c 'd)))))
  (test x)) ;#f

;clearly there are cycles
;---
(let* ((x (list 'b))
       (a (cons 'a x)))
  (set-cdr! x x)
  (test a)) ;#t
;---
(let* ((x '(a b c d))
       (y (make-cycle x)))
  (test y)) ;#t

;troublesome cases (there is sharing but no cycles)
;---
(let* ((x '(a))
       (y (cons x x))
       (p1 (list y))
       (p2 (cons y y)))
  (test p1) ;#f
  (test p2)) ;#f

