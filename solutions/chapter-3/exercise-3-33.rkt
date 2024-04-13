
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.33.
;---
;Using primitive multiplier, adder, and constant constraints, define a procedure
;'averager' that takes three connectors 'a', 'b', and 'c' as inputs and establishes the
;constraint that the value of 'c' is the average of the values of 'a' and 'b'.
;------------------------------------------------------------------------------------------

;schematization of the averager constraint system
;---
;c = (a+b)/2 => 2c = a+b
;---
;       +--------+          +--------+
; c --- | m1     |    y     |     a1 | --- a
;       |    * p | -------- | s +    |
;   +-- | m2     |          |     a2 | --- b
;   |   +--------+          +--------+
;   |
;   +------ 2
;      x

;'averager'
;[see "extra-3-constraints.rkt" for missing procedure definitions]
;---
(define (averager a b c)
  (let ((x (make-connector))
        (y (make-connector)))
    (multiplier c x y)
    (adder a b y)
    (constant 2 x)))

;hypothetical test
;---
(let ((a (make-connector))
      (b (make-connector))
      (c (make-connector)))
  ;---
  (averager a b c)
  ;---
  (probe 'a a)
  (probe 'b b)
  (probe 'c c)
  ;---
  (set-value! a 2 'user)
  (display "---") (newline)
  (set-value! b 4 'user)
  (display ""))

