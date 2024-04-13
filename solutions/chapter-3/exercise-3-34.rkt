
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.34.
;---
;Louis Reasoner wants to build a squarer, a constraint device with two terminals such
;that the value of connector 'b' on the second terminal will always be the square of the
;value 'a' on the first terminal. He proposes the following simple device made from a
;multiplier:
;---
;(define (squarer a b)
;  (multiplier a a b))
;---
;There is a serious flaw in this idea. Explain.
;------------------------------------------------------------------------------------------

;Flaw in Reasoner's implementation
;---
;By implementing a squarer based on a single multiplier device, Louis Reasoner incurs in
;a pernicious, yet ilusive mistake. Such a flaw gets away unnoticed if we follow the
;traditional unidirectional paradigm of computation for calculating squares: given an
;argument 'x', the squarer returns x**2 as its value. This is the case, since, when
;argument 'a' is set, both the multiplicand (m1) and the multiplier (m2) are set to that
;same value. That is, we have all the information needed to compute the missing value;
;hence, 'b' is set to a**2. However, if we try to set the value of 'b', the multiplier
;device is not able to compute the value of the remaining connectors. Although the
;missing connectors are essentially the same object (i.e., 'a') the system is not equiped
;to deal with the fact that the value of 'a' is simply the square root of 'b'. For more
;details on why this is the case, see the definition of the internal procedure
;'process-new-value' in 'multiplier' ("extra-3-constraints.rkt").

