
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
;      |   |   |            |              E          |
;      |   |   +--- and- ---+----------------------------- C(arry)
; B -------+------- gate                              |
;      |                                              |
;      +----------------------------------------------+


;------------------------------------------------------------------------------------------
;1st SCENARIO
;(action procedures are not called when added to the action procedure list)
;------------------------------------------------------------------------------------------

;prelude
;---
;calling an action procedure immediately after it is added to the wire's action procedure
;list, adds the corresponding lambda expression (setting the output signal) to the agenda
;('the-agenda'); these lambda expressions initially added to 'the-agenda' may be regarded
;as the circuit's initializers; as we'll see below, not initilizing circuits may have
;rather pernicious effects on subsequent computations; for reasons of simplicity, it is
;assumed that 'the-agenda' is a list of lambda expressions

;(half-adder A B S C)
;---
;- 'the-agenda' stays as is -> '()

;(set-signal! A 1)
;---
;- A's signal is set to 1
;- and-gate and or-gate action procedures are called (the respective lambda expressions
;  are added to 'the-agenda')

;(propagate)
;---
;- tries setting signal C, but fails (A and B -> 0, and D is already 0)
;---
;- F's signal is set to 1 (A or B -> 1)
;- and-gate action procedure (relative to the rightmost and-gate) is called , adding the
;  respective lambda expression to 'the-agenda'
;---
;- tries setting signal S, but fails (E and F -> 0, and S is already 0)

;conclusion
;---
;the signals of the ouput wires, S and C, should be set to 1 and 0, respectively, given
;that the binary addition of 1 (A) and 0 (B) yields a sum of 1 and a carry of 0; the
;absence of circuit "initialization" resulted in both signals being evaluated to 0


;------------------------------------------------------------------------------------------
;2nd SCENARIO
;(action procedures are called when added to the action procedure list)
;------------------------------------------------------------------------------------------

;prelude
;---
;in the present section, we substantiate the need for initializing the circuit's wires
;upon the addition of action procedures to the respective action procedure lists; to do
;so, we track the exact same process as before and compare the results of both schemes

;(half-adder A B S C)
;---
;- 'the-agenda' -> '((lambda () (set-signal! C (logical-and A B)))   ;A -> 0 and B -> 0
;                    (lambda () (set-signal! C (logical-and A B)))   ;A -> 0 and B -> 0
;                    (lambda () (set-signal! F (logical-or A B)))    ;A -> 0 and B -> 0
;                    (lambda () (set-signal! F (logical-or A B)))    ;A -> 0 and B -> 0
;                    (lambda () (set-signal! E (logical-not C)))     ;C -> 0
;                    (lambda () (set-signal! S (logical-and E F)))   ;E -> 0 and F -> 0
;                    (lambda () (set-signal! S (logical-and E F))))  ;E -> 0 and F -> 0

;(set-signal! A 1)
;---
;- A's signal is set to 1
;- 'the-agenda' -> '((lambda () (set-signal! C (logical-and A B)))   ;A -> 0 and B -> 0
;                    (lambda () (set-signal! C (logical-and A B)))   ;A -> 0 and B -> 0
;                    (lambda () (set-signal! F (logical-or A B)))    ;A -> 0 and B -> 0
;                    (lambda () (set-signal! F (logical-or A B)))    ;A -> 0 and B -> 0
;                    (lambda () (set-signal! E (logical-not C)))     ;C -> 0
;                    (lambda () (set-signal! S (logical-and E F)))   ;E -> 0 and F -> 0
;                    (lambda () (set-signal! S (logical-and E F)))   ;E -> 0 and F -> 0
;                    (lambda () (set-signal! C (logical-and A B)))   ;A -> 1 and B -> 0
;                    (lambda () (set-signal! F (logical-or A B))))   ;A -> 1 and B -> 0

;(propagate)
;---
;- tries setting signal C, but fails (A and B -> 0, and D -> 0)
;- 'the-agenda' -> '((lambda () (set-signal! F (logical-or A B)))    ;A -> 0 and B -> 0
;                    (lambda () (set-signal! F (logical-or A B)))    ;A -> 0 and B -> 0
;                    (lambda () (set-signal! E (logical-not C)))     ;C -> 0
;                    (lambda () (set-signal! S (logical-and E F)))   ;E -> 0 and F -> 0
;                    (lambda () (set-signal! S (logical-and E F)))   ;E -> 0 and F -> 0
;                    (lambda () (set-signal! C (logical-and A B)))   ;A -> 1 and B -> 0
;                    (lambda () (set-signal! F (logical-or A B))))   ;A -> 1 and B -> 0
;---
;- F's signal is set to 1 (A or B -> 1)
;- lambda expression is added to 'the-agenda'
;- 'the-agenda' -> '((lambda () (set-signal! E (logical-not C)))     ;C -> 0
;                    (lambda () (set-signal! S (logical-and E F)))   ;E -> 0 and F -> 0
;                    (lambda () (set-signal! S (logical-and E F)))   ;E -> 0 and F -> 0
;                    (lambda () (set-signal! C (logical-and A B)))   ;A -> 1 and B -> 0
;                    (lambda () (set-signal! F (logical-or A B)))    ;A -> 1 and B -> 0
;                    (lambda () (set-signal! S (logical-and E F))))  ;E -> 0 and F -> 1
;---
;- E's signal is set to 1 (not C -> 1)
;- lambda expression is added to 'the-agenda'
;- 'the-agenda' -> '((lambda () (set-signal! S (logical-and E F)))   ;E -> 0 and F -> 1
;                    (lambda () (set-signal! S (logical-and E F)))   ;E -> 0 and F -> 1
;                    (lambda () (set-signal! C (logical-and A B)))   ;A -> 1 and B -> 0
;                    (lambda () (set-signal! F (logical-or A B)))    ;A -> 1 and B -> 0
;                    (lambda () (set-signal! S (logical-and E F)))   ;E -> 0 and F -> 1
;                    (lambda () (set-signal! S (logical-and E F))))  ;E -> 1 and F -> 1
;---
;- tries setting signal S, but fails (E and F -> 0, and S -> 0)
;- 'the-agenda' -> '((lambda () (set-signal! C (logical-and A B)))   ;A -> 1 and B -> 0
;                    (lambda () (set-signal! F (logical-or A B)))    ;A -> 1 and B -> 0
;                    (lambda () (set-signal! S (logical-and E F)))   ;E -> 0 and F -> 1
;                    (lambda () (set-signal! S (logical-and E F))))  ;E -> 1 and F -> 1
;---
;- tries setting signal C, but fails (A and B -> 0, and S -> 0)
;- 'the-agenda' -> '((lambda () (set-signal! F (logical-or A B)))    ;A -> 1 and B -> 0
;                    (lambda () (set-signal! S (logical-and E F)))   ;E -> 0 and F -> 1
;                    (lambda () (set-signal! S (logical-and E F))))  ;E -> 1 and F -> 1
;---
;- tries setting signal F, but fails (A or B -> 1, and F -> 1)
;- 'the-agenda' -> '((lambda () (set-signal! S (logical-and E F)))   ;E -> 0 and F -> 1
;                    (lambda () (set-signal! S (logical-and E F))))  ;E -> 1 and F -> 1
;---
;- tries setting signal S, but fails (E and F -> 0, and S -> 0)
;- 'the-agenda' -> '((lambda () (set-signal! S (logical-and E F))))  ;E -> 1 and F -> 1
;---
;- S's signal is set to 1 (E and F -> 1)

;conclusion
;---
;initializing the circuit paid off as expected: setting the signal of wire A to 1 yielded
;the anticipated result, that is, the signals of wires S and C were set to the values 1
;and 0, respectively

