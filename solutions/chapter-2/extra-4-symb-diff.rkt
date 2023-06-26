
#lang sicp

;------------------------------------------------------------------------------------------
;SYMBOLIC DIFFERENTIATION
;---
;- the input expressions are in standard infix notation and parenthesis might be droped
;where necessary; accordingly, the output expressions (the derivatives) are also in infix
;notation
;---
;- as of now, the program only deals with derivatives of atomic symbols (e.g., x, y, 3,
;etc.), sums, products and exponential expressions with respect to a given variable
;------------------------------------------------------------------------------------------

;------------------------------------------------------------------------------------------
;DERIV (main procedure)
;------------------------------------------------------------------------------------------

;defining 'deriv'
;('deriv' is defined independent of the data representation; this means that, for
;example, given an adequate implementation of predicates, constructors and selectors,
;input expressions might be constructed in either prefix or infix notation)
;---
(define (deriv exp var)
  (cond ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv (multiplicand exp) var))
                   (make-product (deriv (multiplier exp) var)
                                 (multiplicand exp))))
        ((exponential? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponential (base exp)
                                                       (make-sum (exponent exp)
                                                                 -1)))
                       (deriv (base exp) var)))
        (else
         (error "unknown expression type: DERIV" exp))))

;------------------------------------------------------------------------------------------
;PREDICATES
;------------------------------------------------------------------------------------------

;defining predicates for atomic expressions
;---
(define (variable? exp)
  (not (pair? exp)))
;---
(define (same-variable? exp var)
  (and (variable? exp) (variable? var) (eq? exp var)))

;defining the remaining predicates
;(precedence of operations is defined here; when parsing an expression 'exp', 'exp' is
;first split at "operator sites" having the weakest possible binding to the respective
;operands. from weakest to strongest: '+', '*', '^')
;---
(define (memq item x)
  (cond ((null? x) false)
        ((eq? (car x) item) x)
        (else (memq item (cdr x)))))
;---
(define (operator exp)
  (cond ((memq '+ exp) '+)
        ((memq '* exp) '*)
        (else '^)))
;---
(define (sum? exp)
  (and (not (variable? exp))
       (eq? (operator exp) '+)))
;---
(define (product? exp)
  (and (not (variable? exp))
       (eq? (operator exp) '*)))
;---
(define (exponential? exp)
  (and (not (variable? exp))
       (eq? (operator exp) '^)))

;------------------------------------------------------------------------------------------
;CONSTRUCTORS
;------------------------------------------------------------------------------------------

;defining the constructors
;---
(define (=number? exp num)
  (and (number? exp) (= exp num)))
;---
(define (fast-expt b e)
  (define (even? n) (= (remainder n 2) 0))
  (define (square n) (* n n))
  (cond ((= e 0) b)
        ((even? e) (square (fast-expt b (/ e 2))))
        (else (* b (fast-expt b (- e 1))))))
;---
(define (make-sum a1 a2)
  (cond ((and (number? a1) (number? a2))
         (+ a1 a2))
        ((=number? a1 0)
         a2)
        ((=number? a2 0)
         a1)
        (else
         (list a1 '+ a2))))
;---
(define (make-product m1 m2)
  (cond ((and (number? m1) (number? m2))
         (* m1 m2))
        ((or (=number? m1 0) (=number? m2 0))
         0)
        ((=number? m1 1)
         m2)
        ((=number? m2 1)
         m1)
        (else
         (list m1 '* m2))))
;---
(define (make-exponential b e)
  (cond ((=number? b 1)
         1)
        ((and (=number? b 0) (not (=number? e 0)))
         0)
        ((=number? e 1)
         b)
        ((and (=number? e 0) (not (=number? b 0)))
         1)
        ((and (number? b) (number? e)
              (or (not (=number? b 0)) (not (=number? e 0))))
         (fast-expt b e))
        (else
         (list b '^ e))))

;------------------------------------------------------------------------------------------
;SELECTORS
;------------------------------------------------------------------------------------------

;defining general procedures for the construction of selectors
;(selectors for the operand to the left of the operator 'op': construct a sublist of
;'exp' containing all the elements up to and not including the first occurrence of 'op';
;selectors for the operand to the right of the operator 'op': construct a sublist of
;'exp' containing all the elements of 'exp' after the first occurence of 'op')
;---
(define (left-selector exp op)
  (define (construct-left exp)
    (if (eq? (car exp) op)
        '()
        (cons (car exp)
              (construct-left (cdr exp)))))
  (let ((result (construct-left exp)))
    (if (= (length result) 1)
        (car result)
        result)))
;---
(define (right-selector exp op)
  (let ((result (cdr (memq op exp))))
    (if (= (length result) 1)
        (car result)
        result)))

;defining the actual selectors
;---
(define (addend exp) (left-selector exp '+))
(define (augend exp) (right-selector exp '+))
;---
(define (multiplier exp) (left-selector exp '*))
(define (multiplicand exp) (right-selector exp '*))
;---
(define (base exp) (left-selector exp '^))
(define (exponent exp) (right-selector exp '^))

;------------------------------------------------------------------------------------------
;TESTS
;------------------------------------------------------------------------------------------

;test for random expressions
;---
(define (test exp var)
  (display "D[") (display exp) (display "] / D[") (display var) (display "] = ")
  (display (deriv exp var))
  (newline))
;---
(define exp1 '(x + 3 * (x + y + 2)))
(test exp1 'x)
;---
(define exp2 '(a * x ^ 3 + b * x ^ 2 + c * x + d))
(test exp2 'x)

