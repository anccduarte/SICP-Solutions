
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 4.1.
;---
;Notice that we cannot tell whether the metacircular evaluator evaluates operands from
;left to right or from right to left. Its evaluation order is inherited from the
;underlying Lisp: If the arguments to 'cons' in 'list-of-values' are evaluated from left
;to right, then 'list-of-values' will evaluate operands from left to right; and if the
;arguments to 'cons' are evaluated from right to left, then 'list-of-values' will
;evaluate operands from right to left. Write a version of 'list-of-values' that evaluates
;operands from left to right regardless of the order of evaluation in the underlying
;Lisp. Also write a version of 'list-of-values' that evaluates operands from right to
;left.
;------------------------------------------------------------------------------------------

;original 'list-of-values'
;---
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons
       (eval (first-operand exps) env)
       (list-of-values (rest-operands exps) env))))

;'list-of-values-lr'
;---
;Note that the nested 'let' expressions are a must. If the 'let' expressions weren't
;nested, we would be dependent on the order of evaluation of the arguments to 'lambda',
;which, for reasons of consistency, would presumably be the same as in 'cons'. Nesting
;the 'let' expressions forces the first (i.e., leftmost) operand to be evaluated first.
;[Remember that (let ((x a) (y b)) ...) is syntactic sugar for ((lambda (x y) ...) a b),
;hence the limitation regarding the order of evaluation of 'lambda' expressions used by
;the underlying Scheme.] An analogous reasoning applies to 'list-of-values-rl'.
;---
(define (list-of-values-lr exps env)
  (if (no-operands? exps)
      '()
      (let ((left (eval (first-operand exps) env)))
        (let ((right (list-of-values-lr (rest-operands exps) env)))
          (cons left right)))))

;'list-of-values-rl'
;---
(define (list-of-values-rl exps env)
  (if (no-operands? exps)
      '()
      (let ((right (list-of-values-rl (rest-operands exps) env)))
        (let ((left (eval (first-operand exps) env)))
          (cons left right)))))

