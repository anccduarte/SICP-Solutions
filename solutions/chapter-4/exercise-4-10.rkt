
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 4.10.
;---
;By using data abstraction, we were able to write an 'eval' procedure that is independent
;of the particular syntax of the language to be evaluated. To illustrate this, design and
;implement a new syntax for Scheme by modifying the procedures in this section, without
;changing 'eval' or 'apply'.
;------------------------------------------------------------------------------------------

;Prelude
;---
;Scheme exploits prefix notation in expression construction. That is to say that both the
;type of expressions and the operator in combinations are determined by the first symbol
;in the respective expression. Our evaluator ('eval' + 'apply') perfectly deals with this
;kind of expressions. However, it is not limited to such a notation. Since the syntax of
;the language being evaluated is fully determined by the procedures that classify and
;extract pieces of expressions, our evaluator is entirely capable of coping with the
;evaluation of expressions following other notation variants. A widely known variant goes
;by the name of postfix notation, in which the types of expressions or the operator in a
;combination is the rightmost element in the expression. For example, a procedure for
;adding two numbers 'a' and 'b' would be called as (a b +). The task of implementing the
;whole set of syntax procedures is strenuous; hence, we only provide for procedures that
;extract all elements but the last and the rearest element of a combination (i.e., to get
;the type/operator of an expression). The remnant syntax procedures are straightforwardly
;implemented as further selection on the elements yielded by the former procedure [for a
;detailed panorama of these procedures, consider reading SICP's sections "4.1.1 The Core
;of the Evaluator" and "4.1.2 Representing Expressions"]. For an amusing change of the 
;syntax of 'if', visit Mitchell Kember's "https://mk12.github.io/sicp/exercise/4/1.html".

;Extracting elements from postfix combination
;['operands' extracts all elements of a combination except for the last, while 'operator'
;selects the rearmost element of an expression.]
;---
(define (operator exp)
  (err-extract exp 'OPERATOR)
  (let lp ((exp exp))
    (if (null? (cdr exp))
        (car exp)
        (lp (cdr exp)))))
;---
(define (operands exp)
  (err-extract exp 'OPERANDS)
  (let lp ((exp exp))
    (if (null? (cdr exp))
        '()
        (cons (car exp)
              (lp (cdr exp))))))
;---
(define (err-extract exp m)
  (if (null? exp)
      (error m "Unable to operate on '()")))

;Testing 'operator' and 'operands'
;---
(define (test-extract exp)
  (display "expression: ") (display exp) (newline)
  (display "operator: ") (display (operator exp)) (newline)
  (display "operands: ") (display (operands exp)) (newline)
  (display "---") (newline))
;---
(let ((exp1 '(1 2 3 4 +))
      (exp2 '(proc))
      (exp3 '()))
  (test-extract exp1)
  (test-extract exp2)
  (test-extract exp3))

