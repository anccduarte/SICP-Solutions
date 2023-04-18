
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 1.5.
;---
;Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with
;is using applicative-order evaluation or normal-order evaluation. He defines the
;following two procedures (SEE BELOW *). Then he evaluates the expression (test 0 (p)).
;What behavior will Ben observe with an interpreter that uses applicative-order
;evaluation? What behavior will he observe with an interpreter that uses normal-order
;evaluation? Explain your answer. (Assume that the evaluation rule for the special form
;'if' is the same whether the interpreter is using normal or applicative order: The
;predicate expression is evaluated first, and the result determines whether to evaluate
;the consequent or the alternative expression.)
;------------------------------------------------------------------------------------------

;(*) procedures
;---
(define (p) (p))
(define (test x y) (if (= x 0) 0 y))

;applicative-order evaluation -> "evaluate arguments and then apply"
;---
;1. (test 0 (p))
;2. evaluate (p) -> (p) -> (test 0 (p))
;3. evaluate (p) -> (p) -> (test 0 (p))
;... [infinite loop]

;normal-order evaluation -> "fully expand and then reduce"
;---
;1. (test 0 (p))
;2. (if (= 0 0) 0 (p))
;3. 0 [(p) is not evaluated since the predicate is #t]

