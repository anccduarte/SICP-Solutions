
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.54.
;---
;Two lists are said to be 'equal?' if they contain equal elements arranged in the same
;order. For example,
;---
;(equal? '(this is a list) '(this is a list))
;---
;is true, but
;---
;(equal? '(this is a list) '(this (is a) list))
;---
;is false. To be more precise, we can define 'equal?' recursively in terms of the basic
;'eq?' equality of symbols by saying that a and b are 'equal?' if they are both symbols
;and the symbols are 'eq?', or if they are both lists such that (car a) is 'equal?' to
;(car b) and (cdr a) is 'equal?' to (cdr b). Using this idea, implement 'equal?' as a
;procedure.
;------------------------------------------------------------------------------------------

;'equal?' -> version 1
;(maybe too complicated...)
;---
(define (equal? a b)
        ;case 1
  (cond ((and (null? a) (null? b))
         true)
        ;case 2
        ((and (not (pair? (car a))) (not (pair? (car b))))
         (if (eq? (car a) (car b))
             (equal? (cdr a) (cdr b))
             false))
        ;case 3
        ((and (pair? (car a)) (pair? (car b)))
         (if (equal? (car a) (car b))
             (equal? (cdr a) (cdr b))
             false))
        ;otherwise
        (else
         false)))

;'equal2?' -> version 2
;(cleaner implementation - directly from the question formulation)
;---
(define (equal2? a b)
  (or (eq? a b)
      (and (pair? a)
           (pair? b)
           (equal2? (car a) (car b))
           (equal2? (cdr a) (cdr b)))))

;test both versions
;---
(define (test proc)
  (display proc) (newline)
  (display "'(this is a list) '(this is a list) -> ")
  (display (proc '(this is a list) '(this is a list))) (newline)
  (display "'(this is a list) '(this (is a) list) -> ")
  (display (proc '(this is a list) '(this (is a) list))) (newline)
  (display "---") (newline))
;---
(test equal?)
(test equal2?)

