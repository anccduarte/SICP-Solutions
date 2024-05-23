
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 4.2.
;---
;Louis Reasoner plans to reorder the 'cond' clauses in 'eval' so that the clause for
;procedure applications appears before the clause for assignments. He argues that this
;will make the interpreter more efficient: Since programs usually contain more
;applications than assignments, definitions, and so on, his modified 'eval' will usually
;check fewer clauses than the original 'eval' before identifying the type of an
;expression.
;---
;(a) What is wrong with Louis's plan? (Hint: What will Louis's evaluator do with the
;    expression (define x 3)?)
;---
;(b) Louis is upset that his plan didn't work. He is willing to go to any lengths to make
;    his evaluator recognize procedure applications before it checks for most other kinds
;    of expressions. Help him by changing the syntax of the evaluated language so that
;    procedure applications start with 'call'. For example, instead of (factorial 3) we
;    will now have to write (call factorial 3) and instead of (+ 1 2) we will have to
;    write (call + 1 2).
;------------------------------------------------------------------------------------------

;original 'eval' implementation
;---
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))

;(a)
;---
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
;---
;The current implementation of 'application?' may simply be seen as an alias for 'pair?'.
;Such a circumstance is problematic if, as Louis suggests, we test a combination with
;'application?' before assessing whether it fits the remaining special form categories.
;Since all expressions besides variables and self-evaluating expressions are themselves
;pairs, all combinations specifying special forms would be mistaken for an application.
;Specifically, (define x 3) would wrongly try to apply an undefined procedure 'define'
;(note that defining a procedure named 'define' would raise an error since 'define' is a
;reserved word in Scheme) to the set of arguments 'x' and 3. These would obviously make
;no sense at all.

;(b)
;---
;Louis' pretensions may be satisfied without modifying the evaluator simply by altering
;the procedures specifying the syntax for procedure application. Remember that "the
;syntax of the language being evaluated is determined solely by the procedures that
;classify and extract pieces of expressions"; in this spirit, the syntax for procedure
;application may be defined as follows:
;---
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      'false))
;---
(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

