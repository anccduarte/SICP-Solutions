
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 4.4.
;---
;Recall the definitions of the special forms 'and' and 'or' from Chapter 1:
;---
;- 'and': The expressions are evaluated from left to right. If any expression evaluates
;  to false, false is returned; any remaining expressions are not evaluated. If all the
;  expressions evaluate to true values, the value of the last expression is returned. If
;  there are no expressions then true is returned.
;---
;- 'or': The expressions are evaluated from left to right. If any expression evaluates to
;  a true value, that value is returned; any remaining expressions are not evaluated. If
;  all expressions evaluate to false, or if there are no expressions, then false is
;  returned.
;---
;Install 'and' and 'or' as new special forms for the evaluator by defining appropriate
;syntax procedures and evaluation procedures 'eval-and' and 'eval-or'. Alternatively,
;show how to implement 'and' and 'or' as derived expressions.
;------------------------------------------------------------------------------------------


;------------------------------------------------------------------------------------------
;PART 1 - INSTALLING 'and' AND 'or'
;[for practice purposes, all remaining special forms are also included]
;------------------------------------------------------------------------------------------

;'tagged-list?'
;---
(define (tagged-list? exp)
  (if (pair? exp)
      (eq? (car exp) 'and)
      false))

;defining syntax and evaluation procedures for 'and'
;---
(define (and? exp) (tagged-list? exp 'and))
(define (and-args exp) (cdr exp))
(define (no-args? args) (null? args))
(define (first-arg args) (car args))
(define (rest-args args) (cdr args))
;---
(define (and-eval exp env)
  (define (eval-iter args)
    (cond ((no-args? args) 'true)
          ((false? (eval (first-arg args) env)) 'false)
          (else (eval-iter (rest-args args)))))
  (eval-iter (and-args exp)))

;defining syntax and evaluation procedures for 'or'
;['no-args?', 'first-arg' and 'rest-args' are the same as for 'and']
;---
(define (or? exp) (tagged-list? exp 'or))
(define (or-args exp) (cdr exp))
;---
(define (or-eval exp env)
  (define (eval-iter args)
    (cond ((no-args? args) 'false)
          ((true? (eval (first-arg args) env)) 'true)
          (else (eval-iter (rest-args args)))))
  (eval-iter (or-args exp)))

;installing evaluation procedures in a putative special forms table
;---
(put 'special-form 'quoted (lambda (exp env)
                             (text-of-quotation exp)))
(put 'special-form 'assignment eval-assignment)
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
(put 'special-form 'and and-eval)
(put 'special-form 'or or-eval)

;defining 'eval'
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


;------------------------------------------------------------------------------------------
;PART 2 - 'and' AND 'or' AS DERIVED EXPRESSIONS
;['and' and 'or' defined as syntactic transformations of the special form 'if']
;------------------------------------------------------------------------------------------

;'and->if'
;---
(define (and->if exp)
  (expand-and-args (and-args exp)))
;---
(define (expand-and-args args)
  (if (no-args? args)
      'true
      (make-if (first-arg args)
               (expand-and-args (rest-args args))
               'false)))

;'or->if'
;---
(define (or->if exp)
  (expand-and-args (or-args exp)))
;---
(define (expand-or-args args)
  (if (no-args? args)
      'false
      (make-if (first-arg args)
               'true
               (expand-or-args (rest-args args)))))

;reinstalling 'and' and 'or'
;---
(put 'special-form 'and (lambda (exp env)
                          (eval (and->if exp) env)))
(put 'special-form 'or (lambda (exp env)
                         (eval (or->if exp) env)))

