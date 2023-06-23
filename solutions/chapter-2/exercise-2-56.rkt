
#lang sicp


;------------------------------------------------------------------------------------------
;BACKGROUND AND 'DERIV' IMPLEMENTATION
;------------------------------------------------------------------------------------------

;"Symbolic differentiation is of special historical significance in Lisp. It was one of
;the motivating examples behind the development of a computer language for symbol
;manipulation. Furthermore, it marked the beginning of the line of research that led to
;the development of powerful systems for symbolic mathematical work, which are currently
;being used by a growing number of applied mathematicians and physicists."

;"In developing the symbolic-differentiation program, we will follow the same strategy of
;data abstraction that we followed in developing the rational-number system of
;Section 2.1.1. That is, we will first define a differentiation algorithm that operates
;on abstract objects such as "sums", "products", and "variables" without worrying about
;how these are to be represented [WISHFUL THINKING!]. Only afterward will we address the
;representation problem."

;"Observe that the latter two rules [derivative of a sum and derivative of a product] are
;recursive in nature. That is, to obtain the derivative of a sum we first find the
;derivatives of the terms and add them. Each of the terms may in turn be an expression
;that needs to be decomposed. Decomposing into smaller and smaller pieces will eventually
;produce pieces that are either constants or variables, whose derivatives will be either
;0 or 1."

;defining the derivative procedure
;(note that the procedure is expressed in terms of abstract data, giving us flexibility
;on the representation of algebraic expressions)
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
        ;(*) from Exercise 2.56.
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation (base exp)
                                                          (make-sum (exponent exp)
                                                                    -1)))
                       (deriv (base exp) var)))
        (else
         (error "unknown expression type: DERIV" exp))))

;predicates, constructors and selectors used in 'deriv'
;---
(define (variable? exp)
  (not (pair? exp)))
;---
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
;---
;(define (make-sum a1 a2)
;  (list '+ a1 a2))
;---
;(define (make-product m1 m2)
;  (list '* m1 m2))
;---
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
;---
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
;---
(define (addend s)
  (cadr s))
;---
(define (augend s)
  (caddr s))
;---
(define (multiplier p)
  (cadr p))
;---
(define (multiplicand p)
  (caddr p))

;The program produces correct answers, but the outputs are unsimplified. "Our difficulty
;is much like the one we encountered with the rational-number implementation: we haven't
;reduced answers to simplest form. To accomplish the rational-number reduction, we needed
;to change only the constructors and the selectors of the implementation. We can adopt a
;similar strategy here. We won't change 'deriv' at all. Instead, we will change
;'make-sum' [and 'make-product']"

;defining new versions of 'make-sum' and 'make-product'
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

;some examples of usage
;---
(display "SOME EXAMPLES") (newline)
(deriv '(+ x 3) 'x) ;1
(deriv '(* x y) 'x) ;y
(deriv '(* (* x y) (+ x 3)) 'x) ;(+ (* x y) (* y (+ x 3)))

;"Although this is quite an improvement, the third example shows that there is still a
;long way to go before we get a program that puts expressions into a form that we might
;agree is 'simplest'. The problem of algebraic simplification is complex because, among
;other reasons, a form that may be simplest for one purpose may not be for another."


;------------------------------------------------------------------------------------------
;EXERCISE 2.56.
;---
;Show how to extend the basic differentiator to handle more kinds of expressions. For
;instance, implement the differentiation rule
;---
;d(u^n)/dx = n*u^(n-1) * du/dx
;---
;by adding a new clause to the 'deriv' program and defining appropriate procedures
;'exponentiation?', 'base', 'exponent', and 'make-exponentiation'. (You may use the
;symbol '**' to denote exponentiation.) Build in the rules that anything raised to the
;power 0 is 1 and anything raised to the power 1 is the thing itself.
;------------------------------------------------------------------------------------------

;addition of the new clause to 'deriv' ('exponentiation?') ABOVE (*)
;('sub?' could also be implemented given the necessity of a constructor 'make-sub' when
;constructing the derivative of an exponentiation expression; however, this operation may
;also be represented in terms of addition, which is already implemented)

;'exponentiation?'
;---
(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))

;'make-exponentiation' -> constructor
;(3 new rules appart from the ones suggested in the question are provided)
;---
(define (fast-expt a b)
  (define (even? n) (= (remainder n 2) 0))
  (define (square n) (* n n))
  (cond ((= b 0)
         1)
        ((even? b)
         (square (fast-expt a (/ b 2))))
        (else
         (* a (fast-expt a (- b 1))))))
;---
(define (make-exponentiation b e)
        ;original rules
  (cond ((and (=number? e 0) (not (=number? b 0)))
         1)
        ((=number? e 1)
         b)
        ;new rules
        ((and (=number? b 0) (not (=number? e 0)))
         0)
        ((=number? b 1)
         1)
        ((and (number? b) (number? e)
              (or (not (= b 0)) (not (= e 0))))
         (fast-expt b e))
        ;obvious case
        (else
         (list '** b e))))

;'base' and 'exponent' -> selectors
;---
(define (base e)
  (cadr e))
;---
(define (exponent e)
  (caddr e))


;test for random expressions
;---
(newline) (display "EXERCISE 2.56.") (newline)
(define (test exp var)
  (display "(deriv '") (display exp) (display " '") (display var) (display ") -> ")
  (display (deriv exp var)) (newline))
;---
(define e1 '(** x y))
(test e1 'x)
(test e1 'y)
;---
(define e2 '(+ (* a x)
               (** x 2)))
(test e2 'x)
(test e2 'a)

