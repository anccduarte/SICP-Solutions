
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.18.
;---
;Define a procedure 'reverse' that takes a list as argument and returns a list of the
;same elements in reverse order.
;------------------------------------------------------------------------------------------

;version 1 -> recursive (uses an helper procedure 'add-elem')
;---
(define (add-elem lst elem)
  (if (null? lst)
      (cons elem nil)
      (cons (car lst)
            (add-elem (cdr lst) elem))))
;---
(define (reverse-1 lst)
  (if (null? lst)
      lst
      (add-elem (reverse-1 (cdr lst))
                (car lst))))

;version 2 -> iterative
;---
(define (reverse-2 lst)
  (define (iter lst rev)
    (if (null? lst)
        rev
        (iter (cdr lst)
              (cons (car lst) rev))))
  (iter lst nil))

;test both versions
;---
(display "version 1 -> ") (reverse-1 (list 1 4 9 16 25))
(display "version 2 -> ") (reverse-2 (list 1 4 9 16 25))

