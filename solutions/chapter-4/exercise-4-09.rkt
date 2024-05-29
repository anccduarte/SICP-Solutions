
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 4.9.
;---
;Many languages support a variety of iteration constructs, such as 'do', 'for', 'while',
;and 'until'. In Scheme, iterative processes can be expressed in terms of ordinary
;procedure calls, so special iteration constructs provide no essential gain in
;computational power. On the other hand, such constructs are often convenient. Design
;some iteration constructs, give examples of their use, and show how to implement them as
;derived expressions.
;------------------------------------------------------------------------------------------

;prelude
;---
;'while' loops are constituted by two main components: the predicate and the body: while
;the predicate evaluates to true, the body is iteratively executed; 'while' expressions
;take four parameters: 1. a list of pairs variable/values, 2. a predicate determining
;whether the loop must be stoped, 3. a procedure for updating variable values, 4. the
;body of the 'while' loop [note that, if the body is constituted by more than one
;expression, it must be encapsulated in an unparameterized lambda expression and called
;upon the generation of the 'while' loop - see example below]

;syntax for 'while' loops
;---
(define (while? exp) (tagged-list? exp 'while))
(define (while-vars-vals exp) (cadr exp))
(define (while-predicate exp) (caddr exp))
(define (while-update-vars exp) (cadddr exp))
(define (while-body exp) (car (cddddr exp)))
;---
(define (while-vars exp)
  (map car (while-vars-vals exp)))
;---
(define (while-values exp)
  (map cadr (while-vars-vals exp)))

;additional syntax
;[constructors for 'define', 'if', 'begin' and 'lambda' expressions]
;---
(define (make-define var body)
  (list 'define var body))
;---
(define (make-if predicate consequent . alternative)
  (append (list 'if predicate consequent) alternative))
;---
(define (make-begin . seq)
  (cons 'begin seq))
;---
(define (make-lambda vars body)
  (list 'lambda vars body))

;'while->combination'
;---
(define (while->combination exp)
  (make-begin
   (make-define
    'while-proc
    (make-lambda
     (while-vars exp)
     (make-if
      (while-predicate exp)
      (make-begin
       (while-body exp)
       (list
        'while-proc
        (cons
         (while-update-vars exp)
         (while-vars exp)))))))
   (cons
    'while-proc
    (while-values exp))))

;testing 'while->combination'
;---
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
;---
(while->combination '(while ((i 1))
                            (< i 10)
                            (lambda (i) (+ i 1))
                            ((lambda () (display i) (newline)))))
;---
;(begin
;  (define while-proc
;    (lambda (i)
;      (if (< i 10)
;          (begin ((lambda () (display i) (newline)))
;                 (while-proc ((lambda (i) (+ i 1)) i))))))
;  (while-proc 1))

