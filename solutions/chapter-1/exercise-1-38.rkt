
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 1.38.
;---
;In 1737, the Swiss mathematician Leonhard Euler published a memoir De Fractionibus
;Continuis, which included a continued fraction expansion for e-2, where 'e' is the base
;of the natural logarithms. In this fraction, the 'Ni' are all 1, and the 'Di' are
;successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, .... Write a program that uses your
;'cont-frac' procedure from Exercise 1.37 to approximate 'e', based on Euler's expansion.
;------------------------------------------------------------------------------------------

;defining 'cont-frac' (recursive)
;---
(define (cont-frac n d k)
  (define (iter i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i)
                    (iter (inc i))))))
  (iter 1))

;procedure for computing 'e' using Euler's expansion
;---
;1, 2, 1, 1, 4, 1, 1, 6, 1, 1,  8, ...
;1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, ...
;   ^        ^        ^         ^
;---
;if (i+1) % 3 != 0, then di = 1
;if (i+1) % 3 == 0, then di = ((i+1) // 3) * 2
;---
(define (euler-expansion k)
  (cont-frac (lambda (i) 1.0)
             (lambda (i) (if (= (remainder (inc i) 3) 0)
                             (* (/ (inc i) 3) 2.0)
                             1.0))
             k))

;test for some values of k
;---
(define (test k)
  (display "k = ") (display k)
  (display " => ")
  (display "e = ") (display (+ (euler-expansion k) 2))
  (newline))
;---
(test 10)
(test 100)
(test 1000)

