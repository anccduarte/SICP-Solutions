
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.35.
;---
;Redefine 'count-leaves' from Section 2.2.2 as an accumulation:
;---
;(define (count-leaves t)
;  (accumulate <??> <??> (map <??> <??>)))
;------------------------------------------------------------------------------------------

;'count-leaves' -> not in terms of 'accumulate'
;---
(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else
         (+ (count-leaves (car tree))
            (count-leaves (cdr tree))))))

;'accumulate' -> helper
;---
(define (accumulate op initial lst)
  (if (null? lst)
      initial
      (op (car lst)
          (accumulate op initial (cdr lst)))))

;version 1 -> 'count-leaves' in terms of 'accumulate'
;---
(define (count-leaves-accum-1 tree)
  (accumulate (lambda (x y)
                (+ (cond ((null? x) 0)
                         ((not (pair? x)) 1)
                         (else
                          (count-leaves-accum-1 x)))
                   y))
              0
              tree))

;version 2 -> 'count-leaves' in terms of 'accumulate'
;---
(define (count-leaves-accum-2 tree)
  (accumulate +
              0
              (map (lambda (sub-tree)
                     (cond ((null? sub-tree) 0)
                           ((not (pair? sub-tree)) 1)
                           (else
                            (count-leaves-accum-2 sub-tree))))
                   tree)))

;test all implementations
;---
(define (test tree)
  (display "tree -> ") (display tree)
  (newline)
  (display "(count-leaves tree) -> ") (display (count-leaves tree))
  (newline)
  (display "(count-leaves-accum-1 tree) -> ") (display (count-leaves-accum-1 tree))
  (newline)
  (display "(count-leaves-accum-2 tree) -> ") (display (count-leaves-accum-2 tree))
  (newline))
;---
(define tree (list 1 2 (list 3 4 (list 5 6) 7) 8 9 nil))
(test tree)

