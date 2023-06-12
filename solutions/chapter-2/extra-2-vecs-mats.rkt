
#lang sicp

;------------------------------------------------------------------------------------------
;HELPER PROCEDURES
;------------------------------------------------------------------------------------------

;APPEND
;---
(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1)
            (append (cdr l1) l2))))
;---
(display "APPEND") (newline)
(append (list 1 2 3) (list 4 5 6))

;MY-MAP
;---
(define (my-map proc lst)
  (if (null? lst)
      nil
      (cons (proc (car lst))
            (my-map proc (cdr lst)))))
;---
(display "---") (newline) (display "MY-MAP") (newline)
(my-map sqrt (list 1 4 9))

;ACCUMULATE
;---
(define (accumulate op init lst)
  (if (null? lst)
      init
      (op (car lst)
          (accumulate op init (cdr lst)))))
;---
(display "---") (newline) (display "ACCUMULATE") (newline)
(accumulate + 0 (list 1 2 3))

;ACCUMULATE-N
;---
(define (accumulate-n op init lsts)
  (if (null? (car lsts))
      nil
      (cons (accumulate op init (map (lambda (x) (car x)) lsts))
            (accumulate-n op init (map (lambda (x) (cdr x)) lsts)))))
;---
(display "---") (newline) (display "ACCUMULATE-N") (newline)
(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

;------------------------------------------------------------------------------------------
;VECTOR/MATRIX OPERATIONS
;------------------------------------------------------------------------------------------

;OUTER-PRODUCT
;---
(define (outer-product v w)
  (my-map (lambda (x)
            (accumulate + 0 x))
          (my-map (lambda (x)
                    (my-map (lambda (y)
                              (* x y))
                            w))
                  v)))
;---
(display "---") (newline) (display "OUTER-PRODUCT") (newline)
(outer-product (list 1 2 3) (list 4 5 6))

;DOT-PRODUCT
;---
(define (dot-product v w)
  (accumulate +
              0
              (accumulate-n *
                            1
                            (list v w))))
;---
(display "---") (newline) (display "DOT-PRODUCT") (newline)
(dot-product (list 1 2 3) (list 4 5 6))

;MATRIX-MUL-VECTOR
;---
(define (matrix-mul-vector mat vec)
  (my-map (lambda (row)
            (dot-product row vec))
          mat))
;---
(display "---") (newline) (display "MATRIX-MUL-VECTOR") (newline)
(matrix-mul-vector (list (list 1 0 0)
                         (list 0 1 0)
                         (list 0 0 1))
                   (list 1 2 3))

;TRANSPOSE
;---
(define (transpose mat)
  (accumulate-n cons nil mat))
;---
(display "---") (newline) (display "TRANSPOSE") (newline)
(transpose (list (list 1 2 3)
                 (list 4 5 6)
                 (list 7 8 9)))

;MATRIX-MUL-MATRIX
;---
(define (matrix-mul-matrix m n)
  (my-map (lambda (row)
            (matrix-mul-vector (transpose n)
                               row))
          m))
;---
(display "---") (newline) (display "MATRIX-MUL-MATRIX") (newline)
(matrix-mul-matrix (list (list 1 0 0)
                         (list 0 1 0)
                         (list 0 0 1))
                   (list (list 1 2 3)
                         (list 4 5 6)
                         (list 7 8 9)))

