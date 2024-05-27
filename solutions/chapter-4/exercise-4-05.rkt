
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 4.5.
;---
;Scheme allows an additional syntax for 'cond' clauses, (<test> => <recipient>). If
;<test> evaluates to a true value, then <recipient> is evaluated. Its value must be a
;procedure of one argument; this procedure is then invoked on the value of the <test>,
;and the result is returned as the value of the 'cond' expression. For example
;---
;(cond ((assoc 'b '((a 1) (b 2))) => cadr)
;      (else false))
;---
;returns 2. Modify the handling of 'cond' so that it supports this extended syntax.
;------------------------------------------------------------------------------------------

;'cond' syntax
;---
;Note that the only modification needed lies at the level of 'cond' syntax. The syntactic
;tansformation of 'cond' onto 'if' remains unaltered. [For reasons of simplicity, we only
;consider 'cond' clauses whose actions are constituted by a single expression.]
;---
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
;---
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
;---
(define (cond-predicate clause) (car clause))
;---
(define (cond-action clause)
  ;---
  (define (cond-recipient? action)
    (eq? (car action) '=>))
  ;---
  (define (apply-action action predicate)
    (list (cadr action) predicate))
  ;---
  (let ((predicate (cond-predicate clause))
        (action (cdr clause)))
    (if (cond-recipient? action)
        (apply-action action predicate) ;application of consequent (except '=>)
        (car action)))) ;only the consequent is considered

;'cond->if' [syntactic transformation of 'cond' onto 'if']
;---
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
;---
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (cond-action first)
                (error "ELSE clause isn't last: COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (cond-action first)
                     (expand-clauses rest))))))

;testing 'cond->if'
;---
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
;---
(define (make-if pred conseq alter)
  (list 'if pred conseq alter))
;---
(cond->if '(cond ((< 2 1) 5)
                 ((< 4 1) 7)
                 ((1 2) => car)
                 (else nice)))

