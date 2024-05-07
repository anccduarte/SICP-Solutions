
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.61.
;---
;Let S be a power series (Exercise 3.59) whose constant term is 1. Suppose we want to
;find the power series 1/S, that is, the series X such that S * X = 1. Write S = 1 + SR,
;where SR is the part of S after the constant term. Then we can solve for X as follows:
;---
;       S * X = 1,
;(1 + SR) * X = 1,
;  X + SR * X = 1,
;           X = 1 - SR * X.
;---
;In other words, X is the power series whose constant term is 1 and whose higher-order
;terms are given by the negative of SR times X. Use this idea to write a procedure
;'invert-unit-series' that computes 1/S for a power series S with constant term 1. You
;will need to use 'mul-series' from Exercise 3.60.
;------------------------------------------------------------------------------------------

;'invert-unit-series'
;---
;X = 1 / S => S * X = 1 => (*) (1 + SR) * X = 1 => X + SR * X = 1 => X = 1 - SR * X
;[S is a power series whose first term is 1, i.e., S = 1 + SR]
;---
;X  -> (invert-unit-series S)
;SR -> (stream-cdr S)
;---
(define (invert-unit-series S)
  (cons-stream
   1
   (scale-stream
    (mul-series (stream-cdr S)
                (invert-unit-series S))
    -1)))

