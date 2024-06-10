
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 4.22.
;---
;Extend the evaluator in this section to support the special form 'let'. (See Exercise
;4.6.)
;------------------------------------------------------------------------------------------

;Prelude
;---
;Both the syntax procedures constructed for 'let' and the procedures effecting the
;syntactic transformation of 'let' onto 'lambda' do not need to be chnaged at all. Here's
;the beauty of conventional interfaces: the original and the new 'eval' procedures share
;the entirety of the low level abstractions. Nonetheless, we still need to add an extra
;clause to 'analyze' specifying the actions to be taken whenever a 'let' expression is
;encountered in our programs.

;'analyze'
;[the definition below is quoted so that the 'let->lambda' test runs frictionlessly]
;---
'(define (analyze exp)
   (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
         ((quoted? exp) (analyze-quoted exp))
         ((variable? exp) (analyze-variable exp))
         ((assignment? exp) (analyze-assignment exp))
         ((definition? exp) (analyze-definition exp))
         ((if? exp) (analyze-if exp))
         ((lambda? exp) (analyze-lambda exp))
         ((let? exp) (analyze (let->lambda exp)))
         ((begin? exp) (analyze-sequence (begin-actions exp)))
         ((cond? exp) (analyze (cond->if exp)))
         ((application? exp) (analyze-application exp))
         (else (error "Unknown expression type: ANALYZE" exp))))

;Syntax for 'let'
;---
(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-body exp) (cddr exp))
;---
(define (let-vars bindings) (map car bindings))
(define (let-vals bindings) (map cadr bindings))

;'let' as a derived expression
;---
(define (let->lambda exp)
  (let ((bindings (let-bindings exp)))
    (cons (make-lambda (let-vars bindings)
                       (let-body exp))
          (let-vals bindings))))

;Testing 'let->lambda'
;---
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))
;---
(define (make-lambda parameters body)
  (cons 'lambda
        (cons parameters
              body)))
;---
(let ((let-exp '(let ((a 1) (b 2))
                  (+ a b))))
  (let->lambda let-exp))
;---
((lambda (a b) (+ a b)) 1 2)

