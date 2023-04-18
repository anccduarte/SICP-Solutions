
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 1.4.
;---
;Observe that our model of evaluation allows for combinations whose operators are
;compound expressions. Use this observation to describe the behavior of the following
;procedure (SEE BELOW *).
;------------------------------------------------------------------------------------------

;(*) procedure definition
;---
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;test for random arguments
;---
(a-plus-abs-b 1 2)
(a-plus-abs-b 1 -2)

;;a + abs(b)
;the 'if' clause is used to determine whether the operation to be carried out over the
;operands 'a' and 'b' is '+' or '-'. if b < 0, use the operator '-'. otherwise, use the
;operator '+'. assume that c is the absolute value of b. so, if b < 0, b = -c. then,
;a - b = a - (-c) = a + c. on the other hand, if b > 0, b = c. then, a + b = a + c. in
;both cases, the result of the operation is a + c = a + abs(b)

