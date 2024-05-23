
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 4.3.
;---
;Rewrite eval so that the dispatch is done in data-directed style. Compare this with the
;data-directed differentiation procedure of Exercise 2.73. (You may use the 'car' of a
;compound expression as the type of the expression, as is appropriate for the syntax
;implemented in this section.)
;------------------------------------------------------------------------------------------

;installing procedures on a putative table of special forms
;---
(put 'special-form 'quote (lambda (exp env)
                            (text-of-quotation exp)))
(put 'special-form 'assigment eval-assignment)
(put 'special-form 'definition eval-definition)
(put 'special-form 'if eval-if)
(put 'special-form 'lambda (lambda (exp env)
                             (make-procedure
                              (lambda-parameters exp)
                              (lambda-body exp)
                              env)))
(put 'special-form 'begin (lambda (exp env)
                            (eval-sequence (begin-actions exp) env)))
(put 'special-form 'cond (lambda (exp env)
                           (eval (cond->if exp) env)))

;data-directed 'eval'
;---
(define (proc exp)
  (get 'special-form (car exp)))
;---
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((proc exp) ((proc exp) exp env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))

