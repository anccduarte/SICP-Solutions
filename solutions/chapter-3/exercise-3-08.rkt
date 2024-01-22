
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 3.8.
;---
;When we defined the evaluation model in Section 1.1.3, we said that the first step in
;evaluating an expression is to evaluate its subexpressions. But we never specified the
;order in which the subexpressions should be evaluated (e.g., left to right or right to
;left). When we introduce assignment, the order in which the arguments to a procedure are
;evaluated can make a difference to the result. Define a simple procedure 'f' such that
;evaluating
;---
;(+ (f 0) (f 1))
;---
;will return 0 if the arguments to '+' are evaluated from left to right but will return 1
;if the arguments are evaluated from right to left.
;------------------------------------------------------------------------------------------

;'f' -> it assumes that (+ (f 0) (f 1)) is evaluated a single time: either from left to
;right or from right to left; if (+ (f 0) (f 1)) is first evaluated from left to right
;and then reevaluated from right to left, it will not produce the desired result the
;second time around: this is due to the fact that evaluating the subexpression (f 0)
;permanently binds 'init' to the value 0
;---
(define f
  (let ((init 1))
    (lambda (n)
      (set! init (* init n))
      init)))

