
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.29.
;---
;Another way to construct an or-gate is as a compound digital logic device, built from
;and-gates and inverters. Define a procedure 'or-gate' that accomplishes this. What is
;the delay time of the or-gate in terms of 'and-gate-delay' and 'inverter-delay'?
;------------------------------------------------------------------------------------------

;logic table
;---
;  A | B | A or B | !A and !B | !(!A and !B)
; -------------------------------------------
;  T | T |   T   |      F     |      T
;  T | F |   T   |      F     |      T
;  F | T |   T   |      F     |      T
;  F | F |   F   |      T     |      F

;schematic representation of the or-gate
;---
;       +--------------------------------------------+
;       |          w1                                | 
; a1 ------- inv -------+               w3           |
;       |               +------- and- ------- inv ------- C
;       |               +------- gate                |
; a2 ------- inv -------+                            |
;       |          w2                                |
;       +--------------------------------------------+

;'or-gate'
;[or-gate-delay = and-gate-delay + 2 * inverter-delay]
;---
(define (or-gate a1 a2 output)
  (let ((w1 (make-wire)) (w2 (make-wire)) (w3 (make-wire)))
    (inverter a1 w1)
    (inverter a2 w2)
    (and-gate w1 w2 w3)
    (inverter w3 output)))

;or-gate delay
;---
;the or-gate delay corresponds to the sum of 'and-gate-delay' and twice the value of
;'inverter-delay'; for an or-gate to be "triggered" solely one input wire (either 'a1' or
;'a2') needs to switch state, hence the multiplier of 'inverter-delay' (2 instead of 3)

