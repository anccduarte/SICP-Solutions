
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.31.
;---
;The internal procedure 'accept-action-procedure!' defined in 'make-wire' specifies that
;when a new action procedure is added to a wire, the procedure is immediately run.
;Explain why this initialization is necessary. In particular, trace through the
;half-adder example in the paragraphs above and say how the system's response would
;differ if we had defined 'accept-action-procedure!' as
;---
;(define (accept-action-procedure! proc)
;  (set! action-procedures (cons proc
;                                action-procedures)))
;------------------------------------------------------------------------------------------


;------------------------------------------------------------------------------------------
;TRUTH TABLES AND SCHEMATIZATION
;------------------------------------------------------------------------------------------

;half-adder truth table for S(um)
;---
;  A | B | A or B | A and B | !(A and B) | (A or B) and !(A and B)
; ---+---+--------+---------+------------+-------------------------
;  T | T |   T    |    T    |     F      |           F
;  T | F |   T    |    F    |     T      |           T
;  F | T |   T    |    F    |     T      |           T
;  F | F |   F    |    F    |     T      |           F

;half-adder truth table for C(arry)
;---
;  A | B | A and B
; ---+---+---------
;  T | T |    T
;  T | F |    F
;  F | T |    F
;  F | F |    F

;half-adder schematization
;---
;      +----------------------------------------------+
;      |                           F                  |
; A -----------+--- or-  -------------------- and- ------- S(um)
;      |   +---|--- gate    +--- inverter --- gate    |
;      |   |   |            | D            E          |
;      |   |   +--- and- ---+----------------------------- C(arry)
; B -------+------- gate                              |
;      |                                              |
;      +----------------------------------------------+


;------------------------------------------------------------------------------------------
;1st SCENARIO
;(action procedures are not initialized when added)
;------------------------------------------------------------------------------------------

;tracing procedure calls
;---
;      | init half-adder | (set-signal! A 1) | (set-signal! B 1)
; -----+-----------------+-------------------+-------------------
;   A  |       0         |         1         |         1
;   B  |       0         |         0         |         1
;   D  |       0         |         0         |         1
;   E  |       0         |         1         |         0
;   F  |       0         |         0         |         1
;   S  |       0         |     --> 0 <--     |         0
;   C  |       0         |         0         |         1

;analyzing the results
;---
;assuming that the action procedures are not executed when added to the action procedure
;list of a wire, any abstraction (e.g., 'half-adder') built on top of the circuit
;language primitives (i.e., 'inverter', 'or-gate' and 'and-gate') is to have all its
;inherent wires initialized to 0; the pernicious effects of such a circumstance is well
;noted upon studying the behavior of 'half-adder': setting signal A to 1 and leaving
;signal B as is should produce a sum (S) of 1 and a carry (C) of 0; however, because
;signal E is left untouched upon initialization of the circuit, S (the result of E and F
;logical-and) outputs 0 instead of 1; the nocive effects of not executing action
;procedures upon initialization are dissipated after setting signal B to 1 (i.e., all
;wires output as expected)


;------------------------------------------------------------------------------------------
;2nd SCENARIO
;(action procedures are initialized when added)
;------------------------------------------------------------------------------------------

;tracing procedure calls
;---
;      | init half-adder | (set-signal! A 1) | (set-signal! B 1)
; -----+-----------------+-------------------+-------------------
;   A  |       0         |         1         |         1
;   B  |       0         |         0         |         1     
;   D  |       0         |         0         |         1
;   E  |       1         |         1         |         0
;   F  |       0         |         1         |         1
;   S  |       0         |         1         |         0
;   C  |       0         |         0         |         1

;analyzing the results
;---
;if action procedures are executed when added to the action procedure list, all wires
;ouput as expected:
;1. if A and B are 0, both the sum (S) and the carry (C) evaluate to 0;
;2. if A is 1 and B is 0, S evaluates to 1 and C to 0
;3. if A and B are 1, S evaluates to 0 and C to 1

