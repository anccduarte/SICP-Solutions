
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.37.
;---
;Suppose we represent vectors v = (vi) as sequences of numbers, and matrices m = (mij) as
;sequences of vectors (the rows of the matrix). For example, the matrix
;---
;|1 2 3 4|
;|4 5 6 6|
;|6 7 8 9|
;---
;is represented as the sequence ((1 2 3 4) (4 5 6 6) (6 7 8 9)). With this
;representation, we can use sequence operations to concisely express the basic matrix and
;vector operations. These operations (which are described in any book on matrix algebra)
;are the following:
;---
;(dot-product v w) returns the sum Σ[i](vi*wi)
;(matrix-*-vector m v) returns the vector t, where ti = Σ[j](mij*vj)
;(matrix-*-matrix m n) returns the matrix p, where pij = Σ[k](mik*nkj)
;(transpose m) returns the matrix n, where nij = mji
;---
;We can define the dot product as (SEE BELOW *). Fill in the missing expressions in the
;following procedures (SEE BELOW **) for computing the other matrix operations. (The
;procedure 'accumulate-n' is defined in Exercise 2.36.)
;------------------------------------------------------------------------------------------

;'accumulate' -> helper
;---
(define (accumulate op init lst)
  (if (null? lst)
      init
      (op (car lst)
          (accumulate op init (cdr lst)))))

;(*) 'dot-product'
;---
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;'accumulate-n' -> helper
;---
(define (accumulate-n op init s)
  (if (null? (car s))
      nil
      (cons (accumulate op
                        init
                        (map (lambda (x) (car x)) s))
            (accumulate-n op
                          init
                          (map (lambda (x) (cdr x)) s)))))

;(**) 'matrix-*-vector'
;---
(define (matrix-*-vector m v)
  (map (lambda (r)
         (dot-product r v))
       m))

;(**) 'transpose'
;---
(define (transpose m)
  (accumulate-n cons nil m))

;(**) 'matrix-*-matrix-1' -> (dot-product r c) for r in m for c in (transpose n)
;---
(define (matrix-*-matrix-1 m n)
  (map (lambda (r)
         (map (lambda (c)
                (dot-product r c))
              (transpose n)))
       m))

;(**) 'matrix-*-matrix-2' -> (matrix-*-vector (transpose n) r) for r in m
;---
(define (matrix-*-matrix-2 m n)
  (map (lambda (r)
         (matrix-*-vector (transpose n) r))
       m))

;test
;---
(display "DOT-PRODUCT") (newline)
(define v (list 1 1 1 1))
(define w (list 1 2 3 4))
(dot-product v w)
;---
(display "---") (newline) (display "MATRIX-*-VECTOR") (newline)
(define m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(matrix-*-vector m v)
;---
(display "---") (newline) (display "TRANSPOSE") (newline)
(transpose m)
;---
(display "---") (newline) (display "MATRIX-*-MATRIX") (newline)
(define mat (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(display "v1 -> ") (matrix-*-matrix-1 mat mat)
(display "v2 -> ") (matrix-*-matrix-2 mat mat)

