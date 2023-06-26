
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.62.
;---
;Give a O(n) implementation of 'union-set' for sets represented as ordered lists.
;------------------------------------------------------------------------------------------

;'union-set' on ordered sets
;---
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1))
               (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1 (union-set (cdr set1) (cdr set2))))
                 ((< x1 x2)
                  (cons x1 (union-set (cdr set1) set2)))
                 (else
                  (cons x2 (union-set set1 (cdr set2)))))))))

;'union-set-alt' -> alternative implementation with less repetition
;(inspired by Gera's solution -> http://community.schemewiki.org/?sicp-ex-2.62)
;---
(define (union-set-alt set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1))
               (x2 (car set2)))
           (cons (min x1 x2)
                 (union-set-alt (if (> x1 x2)
                                    set1
                                    (cdr set1))
                                (if (> x2 x1)
                                    set2
                                    (cdr set2))))))))

;test
;---
(define (test set1 set2)
  (define (display-two x y)
    (display x) (display " ") (display y))
  (display "(union-set ") (display-two set1 set2) (display ") -> ")
  (display (union-set set1 set2))
  (newline)
  (display "(union-set-alt ") (display-two set1 set2) (display ") -> ")
  (display (union-set-alt set1 set2))
  (newline) (display "---") (newline))
;---
(test '() '())
(test '(1 2 3 4) '())
(test '() '(1 2 3 4))
(test '(1 2 3 4) '(3 4 5 6))
(test '(1 2 3 4) '(0 1 2 3))

