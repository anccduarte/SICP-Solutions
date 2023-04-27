
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 1.12.
;---
;The following pattern of numbers is called Pascal's triangle.
;1
;1 1
;1 2 1
;1 3 3 1
;1 4 6 4 1
;...
;The numbers at the edge of the triangle are all 1, and each number inside the triangle
;is the sum of the two numbers above it. Write a procedure that computes elements of
;Pascal's triangle by means of a recursive process.
;------------------------------------------------------------------------------------------

;procedure for computing the element at a specific position (row and col)
;base cases: if row=1 => 1, if col=1 => 1, if row=col => col
;other than that: pascal(row, col) = pascal(row-1, col-1) + pascal(row-1, col)
;---
(define (pascal row col)
  (cond ((= row 1) 1)
        ((= col 1) 1)
        ((= row col) 1)
        (else
         (+ (pascal (dec row) (dec col))
            (pascal (dec row) col)))))

;procedure for printing a given row of the pascal triangle
;---
(define (print-pascal-row row)
  (define (iter-col col)
    (cond ((> col row) (newline))
          (else
           (display (pascal row col))
           (display " ")
           (iter-col (inc col)))))
  (iter-col 1))

;procedure for printing the first n rows of the pascal triangle
;---
(define (print-pascal n)
  (define (iter-row row)
    (cond ((> row n) (display ""))
          (else
           (print-pascal-row row)
           (iter-row (inc row)))))
  (iter-row 1))

;test 'print-pascal' for random n
;---
(print-pascal 5)

