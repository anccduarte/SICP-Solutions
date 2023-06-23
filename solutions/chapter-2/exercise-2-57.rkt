
#lang sicp

;------------------------------------------------------------------------------------------
;EXERCISE 2.57.
;---
;Extend the differentiation program to handle sums and products of arbitrary numbers of
;(two or more) terms. Then, the last example above could be expressed as
;---
;(deriv '(* x y (+ x 3)) 'x)
;---
;Try to do this by changing only the representation for sums and products, without
;changing the 'deriv' procedure at all. For example, the 'addend' of a sum would be the
;first term, and the 'augend' would be the sum of the rest of the terms.
;------------------------------------------------------------------------------------------

;rewriting 'deriv'
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
        (else
         (error "unknown expression type: DERIV" exp))))

;unchanged representation (predicates and constructors)
;---
(define (variable? x)
  (not (pair? x)))
;---
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
;---
(define (sum? x)
  (and (not (variable? x)) (eq? (car x) '+)))
;---
(define (product? x)
  (and (not (variable? x)) (eq? (car x) '*)))
;---
(define (=number? exp num)
  (and (number? exp) (= exp num)))
;---
(define (make-sum a1 a2)
  (cond ((and (number? a1) (number? a2))
         (+ a1 a2))
        ((=number? a1 0)
         a2)
        ((=number? a2 0)
         a1)
        (else
         (list '+ a1 a2))))
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
         (list '* m1 m2))))

;new representation (selectors)
;(note that 'addend' and 'multiplier' remain unchanged)
;---
(define (addend s)
  (cadr s))
;---
(define (augend s)
  (define (compose rest)
    (if (= (length rest) 1)
        (car rest)
        (make-sum (car rest)
                  (compose (cdr rest)))))
  (compose (cddr s)))
;---
(define (multiplier p)
  (cadr p))
;---
(define (multiplicand p)
  (define (compose rest)
    (if (= (length rest) 1)
        (car rest)
        (make-product (car rest)
                      (compose (cdr rest)))))
  (compose (cddr p)))

;by attentively inspecting the latter implementations of 'addend' and 'multiplicand', an
;evident resemblance of the structure of these procedures and 'accumulate' seems to
;emerge. by taking advantage of this, 'addend' and 'multiplicand' can be easily
;implemented as:
;---
(define (accumulate op init lst)
  (if (null? lst)
      init
      (op (car lst)
          (accumulate op init (cdr lst)))))
;---
(define (augend-acc s)
  (accumulate make-sum 0 (cddr s)))
;---
(define (multiplicand-acc p)
  (accumulate make-product 1 (cddr p)))

;test for the above example
;---
(define (test exp var)
  (display "(deriv '") (display exp) (display " '") (display var) (display ") -> ")
  (display (deriv exp var))
  (newline))
;---
(test '(* (* x y) (+ x 3)) 'x)
(test '(* x y (+ x 3)) 'x)
;---
(test '(* (* x y) (+ x 3)) 'y)
(test '(* x y (+ x 3)) 'y)

