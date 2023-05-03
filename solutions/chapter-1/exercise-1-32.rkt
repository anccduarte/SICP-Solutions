
#lang sicp


;------------------------------------------------------------------------------------------
;EXERCISE 1.32.
;---
;a. Show that 'sum' and 'product' (Exercise 1.31) are both special cases of a still more
;general notion called 'accumulate' that combines a collection of terms, using some
;general accumulation function:
;(accumulate combiner null-value term a next b)
;'accumulate' takes as arguments the same term and range specifications as 'sum' and
;product, together with a 'combiner' procedure (of two arguments) that specifies how the
;current term is to be combined with the accumulation of the preceding terms and a
;'null-value' that specifies what base value to use when the terms run out. Write
;'accumulate' and show how 'sum' and 'product' can both be defined as simple calls to
;'accumulate'.
;---
;b. If your 'accumulate' procedure generates a recursive process, write one that
;generates an iterative process. If it generates an iterative process, write one that
;generates a recursive process.
;------------------------------------------------------------------------------------------


;---
;PART A
;---

;defining 'sum' and 'product' as separate concepts
;---
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
;---
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

;defining 'accumulate' -> recursive ('accum-rec')
;---
(define (accum-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accum-rec combiner
                           null-value
                           term
                           (next a)
                           next
                           b))))

;defining 'sum' and 'product' in terms of 'accumulate'
;---
(define (sum-accum term a next b)
  (accum-rec + 0 term a next b))
;---
(define (product-accum term a next b)
  (accum-rec * 1 term a next b))

;test both versions of 'sum' and 'product'
;---
(display "PART A") (newline)
;---
(display "sum -> sum[i=1,10](i) = ") (sum (lambda (x) x) 1 inc 10)
(display "sum-accum -> sum[i=1,10](i) = ") (sum-accum (lambda (x) x) 1 inc 10)
;---
(display "product -> prod[i=1,6](i) = ") (product (lambda (x) x) 1 inc 6)
(display "product-accum -> prod[i=1,6](i) = ") (product-accum (lambda (x) x) 1 inc 6)


;---
;PART B
;---

;defining 'accumulate' -> iterative ('accum-iter')
;---
(define (accum-iter combiner null-value term a next b)
  (define (iter a res)
    (if (> a b)
        res
        (iter (next a) (combiner res (term a)))))
  (iter a null-value))

;test both implementations of 'accumulate'
;---
(newline) (display "PART B") (newline)
;---
(display "accum-rec -> sum[i=1,10](i) = ") (accum-rec + 0 (lambda (x) x) 1 inc 10)
(display "accum-iter -> sum[i=1,10](i) = ") (accum-iter + 0 (lambda (x) x) 1 inc 10)

