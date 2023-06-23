
#lang sicp


;------------------------------------------------------------------------------------------
;EXERCISE 2.58.
;---
;Suppose we want to modify the differentiation program so that it works with ordinary
;mathematical notation, in which '+' and '*' are infix rather than prefix operators.
;Since the differentiation program is defined in terms of abstract data, we can modify it
;to work with different representations of expressions solely by changing the predicates,
;selectors, and constructors that define the representation of the algebraic expressions
;on which the differentiator is to operate.
;---
;a. Show how to do this in order to differentiate algebraic expressions presented in
;infix form, such as (x + (3 * (x + (y + 2)))). To simplify the task, assume that '+' and
;'*' always take two arguments and that expressions are fully parenthesized.
;---
;b. The problem becomes substantially harder if we allow standard algebraic notation,
;such as (x + 3 * (x + y + 2)), which drops unnecessary parentheses and assumes that
;multiplication is done before addition. Can you design appropriate predicates,
;selectors, and constructors for this notation such that our derivative program still
;works?
;------------------------------------------------------------------------------------------


;'variable?', 'same-variable?' and '=number?' -> common to (a) and (b)
;---
(define (variable? x)
  (not (pair? x)))
;---
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
;---
(define (=number? exp num)
  (and (number? exp) (= exp num)))

;'make-sum' and 'make-product' -> common to (a) and (b)
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

;higher-order procedure for testing both versions of 'deriv'
;---
(define (test deriv-proc exp var)
  (display "D[") (display exp) (display "] / D[") (display var) (display "] = ")
  (display (deriv-proc exp var))
  (newline))


;------------------------------------------------------------------------------------------
;PART A
;------------------------------------------------------------------------------------------

;'deriv'
;---
(define (deriv-a exp var)
  (cond ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum-a? exp)
         (make-sum (deriv-a (addend-a exp) var)
                   (deriv-a (augend-a exp) var)))
        ((product-a? exp)
         (make-sum (make-product (multiplier-a exp)
                                 (deriv-a (multiplicand-a exp) var))
                   (make-product (deriv-a (multiplier-a exp) var)
                                 (multiplicand-a exp))))
        (else
         (error "unknown expression type: DERIV" exp))))

;'sum?' and 'product?'
;---
(define (sum-a? exp)
  (and (not (variable? exp)) (eq? (cadr exp) '+)))
;---
(define (product-a? exp)
  (and (not (variable? exp)) (eq? (cadr exp) '*)))

;'addend', 'multiplier', 'augend', 'multiplicand' -> selectors
;---
(define addend-a car)
(define multiplier-a car)
(define augend-a caddr)
(define multiplicand-a caddr)

;test (a)
;---
(display "PART A") (newline)
;---
(define exp1 '((a * (x * x)) + ((b * x) + c)))
;---
(test deriv-a exp1 'x)
(test deriv-a exp1 'a)
(test deriv-a exp1 'b)
(test deriv-a exp1 'c)


;------------------------------------------------------------------------------------------
;PART B
;(solution from meteorgan -> http://community.schemewiki.org/?sicp-ex-2.58)
;------------------------------------------------------------------------------------------

;'deriv'
;---
(define (deriv-b exp var)
  (cond ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum-b? exp)
         (make-sum (deriv-b (addend-b exp) var)
                   (deriv-b (augend-b exp) var)))
        ((product-b? exp)
         (make-sum (make-product (multiplier-b exp)
                                 (deriv-b (multiplicand-b exp) var))
                   (make-product (deriv-b (multiplier-b exp) var)
                                 (multiplicand-b exp))))
        (else
         (error "unknown expression type: DERIV" exp))))

;'memq' -> helper
;(if 'item' cannot be found in 'x', returns 'false'; otherwise, returns the sublist of
;'x' starting in 'item')
;---
(define (memq item x)
  (cond ((null? x) false)
        ((eq? (car x) item) x)
        (else (memq item (cdr x)))))

;'operation' -> helper
;(if the symbol '+' is contained in the expression 'expr', return '+'; otherwise, return
;the symbol '*')
;---
(define (operation expr) 
  (if (memq '+ expr) 
      '+ 
      '*)) 

;'sum?' and 'product?'
;(any time the symbol '+' is contained in the expression 'expr', 'expr' is considered to
;be a sum; otherwise, the expression is a product -> this simple assumption brilliantly
;takes care of the problem of precedence! if an expression contains both the symbols '+'
;and '*' it first "splits" the expression at '+'. for example, let e = (2 + 3 * x). here,
;the expression is conveninetly split into '2' and '3*x' with '+' serving as operator)
;---
(define (sum-b? expr) 
  (eq? '+ (operation expr)))
;---
(define (product-b? expr) 
  (eq? '* (operation expr))) 

;'addend' and 'augend'
;(note that the addend is the sublist of 'expr' up to and not including the first
;occurrence of the symbol '+'; similarly, the augend is the sublist of 'expr' which
;starts at the element following the first occurrence of the symbol '+')
;---
(define (addend-b expr) 
  (define (iter expr result) 
    (if (eq? (car expr) '+) 
        result 
        (iter (cdr expr) (append result (list (car expr)))))) 
  (let ((result (iter expr '()))) 
    (if (= (length result) 1) 
        (car result) 
        result)))
;---
(define (augend-b expr) 
  (let ((result (cdr (memq '+ expr)))) 
    (if (= (length result) 1) 
        (car result) 
        result))) 
  
;'multiplier' and 'multiplicand'
;(similar reasoning as in 'addend' and 'augend' applies here)
;---
(define (multiplier-b expr) 
  (define (iter expr result) 
    (if (eq? (car expr) '*) 
        result 
        (iter (cdr expr) (append result (list (car expr)))))) 
  (let ((result (iter expr '()))) 
    (if (= (length result) 1) 
        (car result) 
        result)))
;---
(define (multiplicand-b expr) 
  (let ((result (cdr (memq '* expr)))) 
    (if (= (length result) 1) 
        (car result) 
        result)))

;NOTE: the line of thought that allowed to arrive at this implementation should be clear
;by now (given the thorough explanations above). however, it's worthly to emphasize that
;the rule followed by the algorithm is to first take care of all the sums in the
;expression 'expr' (since all the expressions containing the symbol '+' are considered to
;be sums) and only then deal with subexpressions containing the symbol '*'. consider the
;expression (2 + x + y * (2 + x) * x + 3). by sequentially arranging the expression in
;prefix notation, the expression would be "split" as follows:
;---
;1. (2 + x + y * (2 + x) * x + 3)
;2. (+ 2 (x + y * (2 + x) * x + 3))
;3. (+ 2 (+ x (y * (2 + x) * x + 3)))
;4. (+ 2 (+ x (+ (y * (2 + x) * x) 3)))
;5. (+ 2 (+ x (+ (* y ((2 + x) * x)) 3)))
;6. (+ 2 (+ x (+ (* y (* (2 + x) x) 3))))
;7. (+ 2 (+ x (+ (* y (* (+ 2 x) x) 3))))


;test (b)
;---
(newline) (display "PART B") (newline)
;---
(define exp2 '(x + 3 * (x + y + 2)))
;---
(test deriv-b exp2 'x)
(test deriv-b exp2 'y)

