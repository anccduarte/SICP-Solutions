
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.27.
;---
;Modify your 'reverse' procedure of Exercise 2.18 to produce a 'deep-reverse' procedure
;that takes a list as argument and returns as its value the list with its elements
;reversed and with all sublists deep-reversed as well. For example,
;---
;(define x (list (list 1 2) (list 3 4)))
;x -> ((1 2) (3 4))
;---
;(reverse x) -> ((3 4) (1 2))
;---
;(deep-reverse x) -> ((4 3) (2 1))
;------------------------------------------------------------------------------------------

;'reverse'
;---
(define (reverse lst)
  (define (iter lst rev)
    (if (null? lst)
        rev
        (iter (cdr lst)
              (cons (car lst) rev))))
  (iter lst nil))

;'deep-reverse-1' -> (car lst) not bound locally (cumbersome)
;---
(define (deep-reverse-1 lst)
  (define (iter lst res)
    (if (null? lst)
        res
        (iter (cdr lst)
              (cons (if (pair? (car lst))
                        (deep-reverse-1 (car lst))
                        (car lst))
                    res))))
    (iter lst nil))

;'deep-reverse-2' -> (car lst) bound locally to 'head'
;---
(define (deep-reverse-2 lst)
  (define (iter lst rev)
    (if (null? lst)
        rev
        (let ((head (car lst)))
          (iter (cdr lst)
                (cons (if (pair? head)
                          (deep-reverse-2 head)
                          head)
                      rev)))))
  (iter lst nil))

;test both implementations
;---
(define (test lst)
  (display "lst -> ") (display lst) (newline)
  (display "(reverse lst) -> ") (display (reverse lst)) (newline)
  (display "(deep-reverse-1 lst) -> ") (display (deep-reverse-1 lst)) (newline)
  (display "(deep-reverse-2 lst) -> ") (display (deep-reverse-2 lst)) (newline))
;---
(define lst1 (list (list 1 2) (list 3 4)))
(define lst2 (list (list 1 2) 3 4 (list (list 3 4) (list 5 6)) 7 (list 8 9)))
;---
(test lst1)
(display "---") (newline)
(test lst2)

