
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.32.
;---
;The procedures to be run during each time segment of the agenda are kept in a queue.
;Thus, the procedures for each segment are called in the order in which they were added
;to the agenda (first in, first out). Explain why this order must be used. In particular,
;trace the behavior of an and-gate whose inputs change from 0, 1 to 1, 0 in the same
;segment and say how the behavior would differ if we stored a segment's procedures in an
;ordinary list, adding and removing procedures only at the front (last in, first out).
;------------------------------------------------------------------------------------------

;Prelude
;---
;Assume that we have two wires connected to an and-gate: wire A has signal 0 and wire B
;has signal 1. In the Exercise, it is suggested that signal A is modified first, and only
;then signal B is set to its new value. Hence, the signal of the and-gate's output
;evaluates to 1 in a first instance (A -> 1 and B -> 1), and then, upon setting signal B,
;the value of this same signal evaluates to 0 (A -> 1 and B -> 0). Thus, the definitive
;signal of and-gate's output should ultimatly evaluate to 0. Moreover, note that the
;ouput's signal changes state in the process, which results in signal propagation through
;the remaining of the putative circuit encompassing the and-gate. Next, we analyze the
;behavior of the and-gate if we store a segment's procedures in a queue (FIFO) and,
;alternatively, in an ordinary list (LIFO).

;FIFO (queue)
;---
;1.
;signal A -> 0
;signal B -> 1
;signal output -> 0
;queue -> '((lambda () (set-signal! A 1)) (lambda () (set-signal! B 0)))
;---
;2.
;signal A -> 1
;signal B -> 1
;signal output -> 1 [changes state, hence, propagates]
;queue -> '((lambda () (set-signal! B 0)))
;---
;3.
;signal A -> 1
;signal B -> 0
;signal output -> 0 [changes state, hence, propagates]
;queue -> '()
;---
;At the end, the output's signal correctly evaluates to 0. Moreover, as briefly expained
;above, the value of the output's signal changes twice, resulting in signal propagation
;through the circuit encompassing the and-gate.


;LIFO (ordinary list)
;---
;1.
;signal A -> 0
;signal B -> 1
;signal output -> 0
;list -> '((lambda () (set-signal! B 0)) (lambda () (set-signal! A 1)))
;---
;2.
;signal A -> 0
;signal B -> 0
;signal output -> 0
;list -> '((lambda () (set-signal! A 1)))
;---
;3.
;signal A -> 1
;signal B -> 0
;signal output -> 0
;list -> '()
;---
;As with FIFO, the output's signal correctly evaluates to 0. However, the output's signal
;never changes state in the process, which, in turn, results in a lack of signal
;propagation through the putative circuit encompassing the and-gate.

