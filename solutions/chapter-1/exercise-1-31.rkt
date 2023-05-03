
#lang sicp


;------------------------------------------------------------------------------------------
;EXERCISE 1.31.
;---
;a. The 'sum' procedure is only the simplest of a vast number of similar abstractions
;that can be captured as higher-order procedures. Write an analogous procedure called
;'product' that returns the product of the values of a function at points over a given
;range. Show how to define 'factorial' in terms of product. Also use product to compute
;approximations to 'pi' using the formula:
;pi/4 = (2*4*4*6*6*8*...) / (3*3*5*5*7*7*...)
;---
;b. If your product 'procedure' generates a recursive process, write one that generates
;an iterative process. If it generates an iterative process, write one that generates a
;recursive process.
;------------------------------------------------------------------------------------------


;---
;PART A
;---

;defining 'product' -> recursive ('prod-rec')
;---
(define (prod-rec term a next b)
  (if (> a b)
      1
      (* (term a)
         (prod-rec term (next a) next b))))

;defining 'factorial' in terms of 'product'
;---
(define (factorial n)
  (prod-rec (lambda (x) x)
            2
            inc
            n))

;test 'factorial'
;---
(display "FACTORIAL") (newline)
(define (test-factorial n)
  (display "n = ") (display n)
  (display " => ")
  (display "n! = ") (display (factorial n))
  (newline))
;---
(test-factorial 0)
(test-factorial 2)
(test-factorial 4)
(test-factorial 6)

;approximating 'pi'
;---
;odd terms -> (2*4*6*...) / (3*5*7*...)
;even terms -> (4*6*8*...) / (3*5*7*...)
;---
(define (approx-pi n)
  (define (next x) (+ x 2))
  (* (prod-rec (lambda (x) (/ x (inc x)))
               2
               next
               n)
     (prod-rec (lambda (x) (/ x (dec x)))
               4
               next
               n)))

;test 'approx-pi'
;---
(newline) (display "APPROXIMATE PI") (newline)
(define (test-approx-pi n)
  (display "n = ") (display n)
  (display " => ")
  (display "pi = ") (display (* 4.0 (approx-pi n)))
  (newline))
;---
(test-approx-pi 10)
(test-approx-pi 100)
(test-approx-pi 1000)
(test-approx-pi 10000)


;---
;PART B
;---

;defining 'product' -> iterative ('prod-iter')
;---
(define (prod-iter term a next b)
  (define (iter a res)
    (if (> a b)
        res
        (iter (next a) (* res (term a)))))
  (iter a 1))

;test both implementations of 'product' for simple cases
;---
(newline) (display "PROD-REC VS. PROD-ITER") (newline)
(display "prod-rec -> factorial(4) = ") (prod-rec (lambda (x) x) 2 inc 4)
(display "prod-iter -> factorial(4) = ") (prod-rec (lambda (x) x) 2 inc 4)
(display "prod-rec -> factorial(6) = ") (prod-rec (lambda (x) x) 2 inc 6)
(display "prod-iter -> factorial(6) = ") (prod-rec (lambda (x) x) 2 inc 6)

